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
        case None => throw new Exception(s"cannot define ivar in global context")
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
            case Some(method) => ScrubyMethod.invoke(method, Nil, ts => evals(ts, e))
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
          case None => e.withValue(ScrubyNilClass)
        }
        case None => e.withValue(ScrubyNilClass)
      }
    }

    def evalKlassDef(name: Symbol, ts: List[SyntaxTree]): EvaluationState = {
      val EvaluationState(_, klasses, localVars, self) =
        evals(ts, e.withSelf(ScrubyObject('Class)))

      EvaluationState(ScrubySymbol(name), klasses + (name -> self.get), localVars, e.self)
    }

    def evalMethodDef(name: Symbol, params: List[Symbol], ts: List[SyntaxTree]): EvaluationState =
      e.self match {

      case Some(self) => e.withSelf(self.withMethod(name -> ScrubyMethod(params, ts)))
      case None => throw new Exception("cannot define method in global context")
    }

    def evalIf(p: SyntaxTree, yeses: List[SyntaxTree], nos: List[SyntaxTree]): EvaluationState =
      eval(p, e) match {

      case state @ EvaluationState(ScrubyFalseClass | ScrubyNilClass, ks, ls, self) =>
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
          val state = evaldArgs.last

          val EvaluationState(returnValue, _, _, newSelf) = if (msg == 'new) {
            ScrubyConstructor.invoke(
              method,
              evaldArgs.map(_.value),
              ts => evals(
                ts,
                state.withSelf(receivingState.value).withLocalVars(VariableMap.empty)))
          } else {
            ScrubyMethod.invoke(
              method,
              evaldArgs.map(_.value),
              ts => evals(
                ts,
                state.withSelf(receivingState.value).withLocalVars(VariableMap.empty)))
          }

          recvr match {
            case Some(Identifier(name)) =>
              EvaluationState(
                returnValue,
                state.klasses,
                state.localVars + (name -> newSelf.get),
                state.self)
            case Some(IvarIdentifier(name)) =>
              EvaluationState(
                returnValue,
                state.klasses,
                state.localVars,
                Some(state.self.get.withIvar(name -> newSelf.get)))
            case Some(v) => state.withValue(returnValue)
            case None => EvaluationState(returnValue, state.klasses, state.localVars, newSelf)
          }
        case None =>
          throw new Exception(s"undefined method `${msg.toString}' for ${receivingState.value}")
      }
    }

    def evalUnless(p: SyntaxTree, ts: List[SyntaxTree]): EvaluationState = eval(p, e) match {
      case EvaluationState(ScrubyNilClass | ScrubyFalseClass, ks, ls, self) =>
        EvaluationState(ScrubyNilClass, ks, ls, self)
      case state => evals(ts, state)
    }

    t match {
      case LocalVarAssignment(name, value) => evalLocalVarAssignment(name, value)
      case IvarAssignment(name, value) => evalIvarAssignment(name, value)
      case KlassDef(name, ts) => evalKlassDef(name, ts)
      case MethodDef(name, params, ts) => evalMethodDef(name, params, ts)
      case Invocation(recvr, msg, args) => evalInvocation(recvr, msg, args)
      case If(p, yes, no) => evalIf(p, yes, no)
      case Unless(p, statements) => evalUnless(p, statements)
      case IvarIdentifier(name) => evalIvarIdentifier(name)
      case Identifier(name) => evalIdentifier(name)
      case Constant(name) => evalConstant(name)
      case String_(s) => e.withValue(ScrubyString(s))
      case Symbol_(s) => e.withValue(ScrubySymbol(s))
      //case Integer_(n) => evalInteger(n)
      //case Float_(n) => evalFloat(n)
      case True => e.withValue(ScrubyTrueClass)
      case False => e.withValue(ScrubyFalseClass)
      case Nil_ => e.withValue(ScrubyFalseClass)
      case ScrubyObjectContainer(obj) => e.withValue(obj)
      case _ => throw new Exception("unimplemented")
    }
  }
}
