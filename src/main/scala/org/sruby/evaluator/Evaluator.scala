package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object Evaluator {
  // Public members
  def apply(ts: List[AST]): RubyObject = evals(ts).run(EvalState.start).value._2

  // Private members
  private type Evaluation = State[EvalState, RubyObject]

  private def evals(ts: List[AST]): Evaluation = {
    val initialState = State.pure[EvalState, RubyObject](RubyNilClass)

    ts.foldLeft(initialState) { (acc, t) => acc.flatMap { _ => eval(t) } }
  }

  // This method may not be tail recursive -- scala can only perform TCO for the most basic of cases
  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  private def eval(t: AST): Evaluation = {
    def evalIvarAssignment(name: Symbol, value: AST): Evaluation = for {
      result <- eval(value)
      _ <- State.modify[EvalState] { s => s.copy(self = s.self.withIvar(name -> result)) }
    } yield result

    def evalLocalVarAssignment(name: Symbol, value: AST): Evaluation = for {
      result <- eval(value)
      _ <- State.modify[EvalState] { s => s.copy(localVars = s.localVars + (name -> result)) }
    } yield result

    def evalConstant(name: Symbol): Evaluation = State { s =>
      s.klasses.get(name) match {
        case Some(value) => (s, value)
        case None => throw new Exception(s"unitialized constant ${name.toString}")
      }
    }

    def evalIvarIdentifier(name: Symbol): Evaluation =
      State { s => (s, s.self.ivars.get(name).getOrElse(RubyNilClass)) }

    def evalKlassDef(name: Symbol, ts: List[AST]): Evaluation = for {
      // in the future we will do RubyConstant#pop or something like that to get the old scope back
      EvalState(_, _, prevSelf, prevScope) <- State.get[EvalState]
      _ <- State.modify[EvalState] { s =>
        s.copy(klasses = s.klasses + (name -> RubyKlass(name)), self = RubyKlass(name),
          scope = name)
      }
      _ <- evals(ts)
      _ <- State.modify[EvalState] { s =>
        s.copy(klasses = s.klasses + (name -> s.self), self = prevSelf, scope = prevScope)
      }
    } yield RubySymbol(name)

    def getKlass(name: Symbol): Evaluation = State { s =>
      val klass =
        s.klasses.get(name).getOrElse(throw new Exception(s"class ${name.toString} does not exist"))

      (s, klass)
    }

    def evalInstanceMethodDef(name: Symbol, params: List[Symbol], ts: List[AST]): Evaluation = for {
      scope <- State.inspect[EvalState, Symbol](_.scope)
      klass <- getKlass(scope)
      _ <- name match {
        case 'initialize => State.modify[EvalState] { s =>
          val newKlass = klass.withKlassMethod('new -> RubyConstructor(scope, params, ts))
          val newSelf = s.self.withKlassMethod('new -> RubyConstructor(scope, params, ts))

          s.copy(klasses = s.klasses + (s.scope -> newKlass), self = newSelf)
        }
        case _ => State.modify[EvalState] { s =>
          val newKlass = klass.withInstanceMethod(name -> RubyMethod(params, ts))
          val newSelf = s.self.withInstanceMethod(name -> RubyMethod(params, ts))

          s.copy(klasses = s.klasses + (s.scope -> newKlass), self = newSelf)
        }
      }
    } yield RubySymbol(name)

    def evalKlassMethodDef(name: Symbol, params: List[Symbol], ts: List[AST]): Evaluation = for {
      scope <- State.inspect[EvalState, Symbol](_.scope)
      klass <- getKlass(scope)
      _ <- State.modify[EvalState] { s =>
        val newKlass = klass.withKlassMethod(name -> RubyMethod(params, ts))

        s.copy(klasses = s.klasses + (s.scope -> newKlass), self = newKlass)
      }
    } yield RubySymbol(name)

    def evalIf(p: AST, yeses: List[AST], nos: List[AST]): Evaluation = for {
      predicateResult <- eval(p)
      result <- predicateResult match {
        case RubyFalseClass | RubyNilClass => evals(nos)
        case _ => evals(yeses)
      }
    } yield result

    def evalArgs(args: List[AST]): State[EvalState, Seq[RubyObject]] = {
      val initialValue = State.pure[EvalState, Seq[RubyObject]](Seq.empty[RubyObject])

      args.foldLeft(initialValue) { (acc, arg) =>
        eval(arg).flatMap { evaldArg => acc.map(_ ++ Seq(evaldArg)) }
      }
    }

    def evalInvocationWithImplicitReceiver(msg: Symbol, args: List[AST]): Evaluation = for {
      EvalState(_, prevLocalVars, prevSelf, prevScope) <- State.get[EvalState]
      result <- prevLocalVars get msg match {
        case Some(obj) => State.pure[EvalState, RubyObject](obj)
        case None => prevSelf.methods get msg match {
          case Some(method) => method.invoke(evalArgs(args), evals)
          case None => throw new Exception(s"undefined local variable or method `${msg.toString}'")
        }
      }
      _ <- State.modify[EvalState](_.copy(localVars = prevLocalVars))
    } yield result

    def evalInvocationWithReceiver(recvr: AST, msg: Symbol, args: List[AST]): Evaluation = for {
      evaldReceiver <- eval(recvr)
      EvalState(prevKlasses, prevLocalVars, prevSelf, prevScope) <- State.get[EvalState]
      result <- evaldReceiver.methods.get(msg) match {
        case Some(method) => for {
          evaldArgs <- evalArgs(args)
          _ <- State.modify[EvalState](_.copy(self = evaldReceiver, scope = evaldReceiver.klass))
          result <- method.invoke(State.pure(evaldArgs), evals)
          EvalState(_, _, newSelf, _) <- State.get[EvalState]
          _ <- recvr match {
            case InvocationWithImplicitReceiverNode(name, _) => State.modify[EvalState] { s =>
              s.copy(localVars = prevLocalVars + (name -> newSelf), self = prevSelf,
                scope = prevScope)
            }
            case IvarIdentifierNode(name) => State.modify[EvalState] { s =>
              s.copy(localVars = prevLocalVars, self = s.self.withIvar(name -> newSelf),
                scope = prevScope)
            }
            case ConstantNode(name) => State.modify[EvalState] { s =>
              s.copy(klasses = prevKlasses + (name -> newSelf), localVars = prevLocalVars,
                self = prevSelf, scope = prevScope)
            }
            case _ => State.modify[EvalState](_.copy(localVars = prevLocalVars, self = prevSelf,
              scope = prevScope))
          }
        } yield result
        case None =>
          throw new Exception(s"undefined method `${msg.toString}' for `${evaldReceiver}'")
      }
    } yield result

    def evalSelf: Evaluation = State.get[EvalState].map(_.self)

    def evalUnless(p: AST, ts: List[AST]): Evaluation = for {
      predicateResult <- eval(p)
      result <- predicateResult match {
        case RubyFalseClass | RubyNilClass => evals(ts)
        case _ => State.pure[EvalState, RubyObject](RubyNilClass)
      }
    } yield result

    t match {
      case LocalVarAssignmentNode(name, value) => evalLocalVarAssignment(name, value)
      case IvarAssignmentNode(name, value) => evalIvarAssignment(name, value)
      case KlassDefNode(name, ts) => evalKlassDef(name, ts)
      case InstanceMethodDefNode(name, params, ts) => evalInstanceMethodDef(name, params, ts)
      case KlassMethodDefNode(name, params, ts) => evalKlassMethodDef(name, params, ts)
      case InvocationWithReceiverNode(recvr, msg, args) =>
        evalInvocationWithReceiver(recvr, msg, args)
      case InvocationWithImplicitReceiverNode(msg, args) =>
        evalInvocationWithImplicitReceiver(msg, args)
      case IfNode(p, yes, no) => evalIf(p, yes, no)
      case UnlessNode(p, statements) => evalUnless(p, statements)
      case IvarIdentifierNode(name) => evalIvarIdentifier(name)
      case IdentifierNode(name) => throw new Exception("attempted to eval an identifier") // eh
      case ConstantNode(name) => evalConstant(name)
      //case StringNode(s) => e.withValue(RubyString(s))
      //case SymbolNode(s) => e.withValue(RubySymbol(s))
      //case Integer_(n) => evalInteger(n)
      //case Float_(n) => evalFloat(n)
      case SelfNode => evalSelf
      case TrueNode => State.pure(RubyTrueClass)
      case FalseNode => State.pure(RubyFalseClass)
      case NilNode => State.pure(RubyNilClass)
      case RubyObjectContainerNode(obj) => State.pure(obj)
      case _ => throw new Exception("unimplemented")
    }
  }
}
