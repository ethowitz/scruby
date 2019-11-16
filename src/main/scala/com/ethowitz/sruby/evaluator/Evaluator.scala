package com.ethowitz.sruby.evaluator

import cats.data.State
import com.ethowitz.sruby.core._
import com.ethowitz.sruby.parser._

object Evaluator {
  // Public methods
  def apply(ts: List[AST]): RubyObject = evals(ts).run(EvalState.start).value._2

  // Private methods
  private def evals(ts: List[AST]): State[EvalState, RubyObject] = {
    val initialState = State.pure[EvalState, RubyObject](RubyNilClass)

    ts.foldLeft(initialState) { (acc, t) => acc.flatMap { _ => eval(t) } }
  }

  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  private def eval(t: AST): State[EvalState, RubyObject] = {
    def evalIvarAssignment(name: Symbol, value: AST): State[EvalState, RubyObject] = for {
      result <- eval(value)
      _ <- State.modify[EvalState](_.addIvarOnSelf(name -> result))
    } yield result

    def evalLocalVarAssignment(name: Symbol, value: AST): State[EvalState, RubyObject] = for {
      result <- eval(value)
      _ <- State.modify[EvalState](_.addLocalVar(name -> result))
    } yield result

    def evalConstant(name: Symbol): State[EvalState, RubyObject] = State { state =>
      val value = state.klasses.get(name).
        getOrElse(throw new Exception(s"unitialized constant ${name.toString}"))

      (state, value)
    }

    def evalIvarIdentifier(name: Symbol): State[EvalState, RubyObject] = State { state =>
      (state, state.self.ivars.get(name).getOrElse(RubyNilClass))
    }

    def evalKlassDef(name: Symbol, ts: List[AST]): State[EvalState, RubyObject] = for {
      prevSelf <- State.inspect[EvalState, RubyObject](_.self)
      _ <- State.modify[EvalState](_.setSelf(RubyObject('Class)))
      _ <- evals(ts)
      EvalState(_, _, newSelf) <- State.get[EvalState]
      _ <- State.modify[EvalState](_.addKlass(name -> newSelf).setSelf(prevSelf))
    } yield RubySymbol(name)

    def evalMethodDef(
      name: Symbol,
      params: List[Symbol],
      ts: List[AST]
    ): State[EvalState, RubyObject] = for {
      _ <- State.modify[EvalState](_.addMethodOnSelf(name -> RubyMethod(params, ts)))
    } yield RubySymbol(name)

    def evalIf(p: AST, yeses: List[AST], nos: List[AST]): State[EvalState, RubyObject] = for {
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

    def evalInvocationWithImplicitReceiver(
      msg: Symbol,
      args: List[AST]
    ): State[EvalState, RubyObject] = for {
      EvalState(_, prevLocalVars, prevSelf) <- State.get
      result: RubyObject <- prevLocalVars get msg match {
        case Some(obj) => State.pure[EvalState, RubyObject](obj)
        case None => prevSelf.methods get msg match {
          case Some(method) => method.invoke(evalArgs(args), evals)
          case None => throw new Exception(s"undefined local variable or method `${msg.toString}'")
        }
      }
      _ <- State.modify[EvalState](_.setLocalVars(prevLocalVars))
    } yield result

    def evalInvocationWithReceiver(
      recvr: AST,
      msg: Symbol,
      args: List[AST]
    ): State[EvalState, RubyObject] = for {
      evaldReceiver <- eval(recvr)
      EvalState(_, prevLocalVars, _) <- State.get[EvalState]
      result <- {
        val method = evaldReceiver.methods.get(msg).getOrElse(
          throw new Exception(s"undefined method `${msg.toString}' for `${evaldReceiver}'")
        )

        method.invoke(evalArgs(args), evals)
      }
      _ <- State.modify[EvalState](_.setLocalVars(prevLocalVars))
    } yield result

    def evalUnless(p: AST, ts: List[AST]): State[EvalState, RubyObject] = for {
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
      case MethodDefNode(name, params, ts) => evalMethodDef(name, params, ts)
      case InvocationWithReceiverNode(recvr, msg, args) =>
        evalInvocationWithReceiver(recvr, msg, args)
      case InvocationWithImplicitReceiverNode(msg, args) =>
        evalInvocationWithImplicitReceiver(msg, args)
      case IfNode(p, yes, no) => evalIf(p, yes, no)
      case UnlessNode(p, statements) => evalUnless(p, statements)
      case IvarIdentifierNode(name) => evalIvarIdentifier(name)
      case IdentifierNode(name) => throw new Exception("attempted to eval an identifier")
      case ConstantNode(name) => evalConstant(name)
      //case StringNode(s) => e.withValue(RubyString(s))
      //case SymbolNode(s) => e.withValue(RubySymbol(s))
      //case Integer_(n) => evalInteger(n)
      //case Float_(n) => evalFloat(n)
      case TrueNode => State.pure(RubyTrueClass)
      case FalseNode => State.pure(RubyFalseClass)
      case NilNode => State.pure(RubyNilClass)
      case RubyObjectContainerNode(obj) => State.pure(obj)
      case _ => throw new Exception("unimplemented")
    }
  }
}
