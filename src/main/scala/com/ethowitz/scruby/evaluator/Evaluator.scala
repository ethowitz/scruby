package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.parser._
import com.ethowitz.scruby.core._

object Evaluator {
  def apply(ts: List[SyntaxTree]): EvaluationState = evals(ts, EvaluationState.start)

  def evals(ts: List[SyntaxTree], e: EvaluationState): EvaluationState = ts match {
    case Nil => e
    case t :: ts => evals(ts, eval(t, e))
  }

  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  def eval(t: SyntaxTree, e: EvaluationState): EvaluationState = {
    def evalIvarAssignment(name: Symbol, value: SyntaxTree): EvaluationState = {
      val EvaluationState(v, ks, ls, self) = eval(value, e)

      self match {
        case Some(s) => EvaluationState(v, ks, ls, Some(s.withIvar(name -> v)))
        case None => throw new Exception("cannot define ivar in global context")
      }
    }

    def evalLocalVarAssignment(name: Symbol, value: SyntaxTree): EvaluationState = {
      val EvaluationState(v, ks, ls, self) = eval(value, e)

      EvaluationState(v, ks, ls + (name -> v), self)
    }

    def evalConstant(name: Symbol): EvaluationState = e.klasses get name match {
      case Some(klass) => e.withValue(klass)
      case None => throw new Exception(s"unitialized constant ${name.toString}")
    }

    def evalIdentifier(name: Symbol): EvaluationState = {
      e.localVars get name match {
        case Some(obj) => e.withValue(obj)
        case None => e.self match {
          case Some(self) => self.methods get name match {
            case Some(method) => RubyMethod.invoke(method, Nil, ts => evals(ts, e))
            case None =>
              throw new Exception(s"undefined local variable or method `${name.toString}'")
          }
          case None => throw new Exception(s"undefined local variable or method `${name.toString}'")
        }
      }
    }

    def evalIvarIdentifier(name: Symbol): EvaluationState = {
      e.self match {
        case Some(s) => s.ivars get name match {
          case Some(value) => e.withValue(value)
          case None => e.withValue(RubyNilClass)
        }
        case None => e.withValue(RubyNilClass)
      }
    }

    def evalKlassDef(name: Symbol, ts: List[SyntaxTree]): EvaluationState = {
      val EvaluationState(_, klasses, localVars, self) =
        evals(ts, e.withSelf(RubyObject('Class)))

      val errorMessage = "scruby bug: class def evaluation resulted in null self"
      EvaluationState(
        RubySymbol(name),
        klasses + (name -> self.getOrElse(throw new Exception(errorMessage))),
        localVars,
        e.self)
    }

    def evalMethodDef(name: Symbol, params: List[Symbol], ts: List[SyntaxTree]): EvaluationState =
      e.self match {
        case Some(self) => e.withSelf(self.withMethod(name -> RubyMethod(params, ts)))
        case None => throw new Exception("cannot define method in global context")
      }

    def evalIf(p: SyntaxTree, yeses: List[SyntaxTree], nos: List[SyntaxTree]): EvaluationState =
      eval(p, e) match {

      case state @ EvaluationState(RubyFalseClass | RubyNilClass, ks, ls, self) =>
        evals(nos, state)
      case state => evals(yeses, state)
    }

    // this method is impossible to understand
    def evalInvocation(
      recvr: Option[SyntaxTree],
      msg: Symbol,
      args: List[SyntaxTree]
    ): EvaluationState = {
      val receivingState = recvr match {
        case Some(r) => eval(r, e)
        case None => e.self match {
          case Some(s) => e.withValue(s)
          case None => throw new Exception("cannot call method with no receiver")
        }
      }

      receivingState.value.methods get msg match {
        case Some(method) =>
          val evaldArgs = args.toSeq.scanLeft(receivingState) { (acc, t) => eval(t, acc) }
          val state = evaldArgs.lastOption match {
            case Some(s) => s
            case None => throw new Exception("Sequence#scanLeft returned an empty sequence")
          }

          val EvaluationState(returnValue, _, _, newSelf) = if (msg == 'new) {
            RubyConstructor.invoke(
              method,
              evaldArgs.map(_.value),
              ts => evals(
                ts,
                state.withSelf(receivingState.value).withLocalVars(VariableMap.empty)))
          } else {
            RubyMethod.invoke(
              method,
              evaldArgs.map(_.value),
              ts => evals(
                ts,
                state.withSelf(receivingState.value).withLocalVars(VariableMap.empty)))
          }

          val errorMessage = "scruby bug: expression evaluation resulted in null self"
          recvr match {
            case Some(IdentifierNode(name)) =>
              EvaluationState(
                returnValue,
                state.klasses,
                state.localVars + (name -> newSelf.getOrElse(throw new Exception(errorMessage))),
                state.self)
            case Some(IvarIdentifierNode(name)) =>
              val selfWithNewIvar = state.self.getOrElse(throw new Exception(errorMessage)).
                withIvar(name -> newSelf.getOrElse(throw new Exception(errorMessage)))

              EvaluationState(
                returnValue,
                state.klasses,
                state.localVars,
                Some(selfWithNewIvar))
            case Some(v) => state.withValue(returnValue)
            case None => EvaluationState(returnValue, state.klasses, state.localVars, newSelf)
          }
        case None =>
          throw new Exception(s"undefined method `${msg.toString}' for ${receivingState.value}")
      }
    }

    def evalUnless(p: SyntaxTree, ts: List[SyntaxTree]): EvaluationState = eval(p, e) match {
      case EvaluationState(RubyNilClass | RubyFalseClass, ks, ls, self) =>
        EvaluationState(RubyNilClass, ks, ls, self)
      case state => evals(ts, state)
    }

    t match {
      case LocalVarAssignmentNode(name, value) => evalLocalVarAssignment(name, value)
      case IvarAssignmentNode(name, value) => evalIvarAssignment(name, value)
      case KlassDefNode(name, ts) => evalKlassDef(name, ts)
      case MethodDefNode(name, params, ts) => evalMethodDef(name, params, ts)
      case InvocationNode(recvr, msg, args) => evalInvocation(recvr, msg, args)
      case IfNode(p, yes, no) => evalIf(p, yes, no)
      case UnlessNode(p, statements) => evalUnless(p, statements)
      case IvarIdentifierNode(name) => evalIvarIdentifier(name)
      case IdentifierNode(name) => evalIdentifier(name)
      case ConstantNode(name) => evalConstant(name)
      case StringNode(s) => e.withValue(RubyString(s))
      case SymbolNode(s) => e.withValue(RubySymbol(s))
      //case Integer_(n) => evalInteger(n)
      //case Float_(n) => evalFloat(n)
      case TrueNode => e.withValue(RubyTrueClass)
      case FalseNode => e.withValue(RubyFalseClass)
      case NilNode => e.withValue(RubyFalseClass)
      case RubyObjectContainerNode(obj) => e.withValue(obj)
      case _ => throw new Exception("unimplemented")
    }
  }
}
