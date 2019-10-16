package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.parser._
import com.ethowitz.scruby.core._
import scala.annotation.tailrec

// TODO: use continuation-passing style for evaluation state machine
// --> already halfway there since evaluation states contain the previously-computed value
// TODO: include syntax tree to evaluate in EvaluationState?
object Evaluator {
  def apply(ts: List[SyntaxTree]) = evals(ts, EvaluationState.start)

  def evals(ts: List[SyntaxTree], e: EvaluationState): EvaluationState = ts match {
    case Nil => e
    case t :: ts => evals(ts, eval(t, e))
  }

  // scalastyle:off cyclomatic.complexity
  def eval(t: SyntaxTree, e: EvaluationState): EvaluationState = {
    def evalAssignment(name: Symbol, value: SyntaxTree): EvaluationState = {
      val EvaluationState(v, ks, ls, self) = eval(value, e)

      EvaluationState(v, ks, ls + (name -> v), self)
    }

    def evalConstant(name: Symbol): EvaluationState = e.klasses get name match {
      case Some(obj) => e.withValue(ScrubySymbol(name))
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

    def evalKlassDef(name: Symbol, ts: List[SyntaxTree]): EvaluationState = {
      val EvaluationState(_, klasses, localVars, self) =
        evals(ts, e.withSelf(ScrubyObject('Class)))

      EvaluationState(ScrubySymbol(name), klasses + (name -> self.get), localVars, e.self)
    }

    def evalMethodDef(name: Symbol, params: List[Symbol], ts: List[SyntaxTree]): EvaluationState = e.self match {
      case Some(obj) => e.withSelf(obj.withMethod(name -> ScrubyMethod(params, ts)))
      case None => throw new Exception("cannot define method in global context")
    }

    def evalIf(p: SyntaxTree, yeses: List[SyntaxTree], nos: List[SyntaxTree]): EvaluationState = {
      val state = eval(p, e)

      state.value match {
        case ScrubyFalseClass | ScrubyNilClass => evals(nos, state)
        case _ => evals(yeses, state)
      }
    }

    // TODO: do one of:
    // 1. change map get methods to throw exceptions themselves
    // 2. keep track of exceptions in EvaluationState and have get methods return an exception state.
    //    this could get complicated since we'd need to check for an exception state after every
    //    evaluation
    // 3. have all eval methods return trys or eithers? I think we want to differentiate between
    //    Scala exceptions and Ruby exceptions
    //
    // maybe it makes sense to treat errors no different from other values. this might make it 
    // easy to implement `rescue` later down the road
    def evalInvocation(recvr: Option[SyntaxTree], msg: Symbol, args: List[SyntaxTree]): EvaluationState = {
      e.localVars get msg match {
        case Some(obj) => e.withValue(obj)
        case None =>
          val receivingState = recvr match {
            case Some(r) => eval(r, e)
            case None => e.self match {
              case Some(s) => e.withValue(s)
              case None => throw new Exception("cannot call method with no receiver")
            }
          }
          receivingState.value.methods get msg match {
            case Some(method) => args match {
              case Nil => ScrubyMethod.invoke(method, Nil, ts => evals(ts, receivingState.withSelf(receivingState.value))).withLocalVars(receivingState.localVars)
              case ts => 
                val evaldArgs = args.scanLeft(receivingState) { (acc, t) => eval(t, acc) }

                ScrubyMethod.invoke(method, evaldArgs.map(_.value), ts => evals(ts, evaldArgs.last.withSelf(receivingState.value))).withLocalVars(evaldArgs.last.localVars)
            }
            case None => throw new Exception(s"undefined method `${msg}' for ${receivingState.value}")
          }
      }
    }

    def evalUnless(p: SyntaxTree, ts: List[SyntaxTree]): EvaluationState = {
      val state = eval(p, e)

      state.value match {
        case ScrubyNilClass | ScrubyFalseClass => state.withValue(ScrubyNilClass)
        case _ => evals(ts, state)
      }
    }

    t match {
      case Assignment(lvalue, rvalue) => evalAssignment(lvalue, rvalue)
      case KlassDef(name, ts) => evalKlassDef(name, ts)
      case MethodDef(name, params, ts) => evalMethodDef(name, params, ts)
      case Invocation(recvr, msg, args) => evalInvocation(recvr, msg, args)
      case If(p, yes, no) => evalIf(p, yes, no)
      case Unless(p, statements) => evalUnless(p, statements)
      case IvarIdentifier(name) => evalIvarIdentifier(name)
      case Identifier(name) => evalIdentifier(name)
      // TODO: fake implementation of constants, probably want a ScrubyConstant derivation of
      // ScrubyObject to allow for assignment of constants to vars. also probably want to store
      // constants in a Set
      case Constant(name) => evalConstant(name) 
      case String_(s) => e.withValue(ScrubyString(s))
      // TODO: fake implementation of Symbol, put em in a Set
      case Symbol_(s) => e.withValue(ScrubySymbol(s))
      //case Integer_(n) => evalInteger(n)
      //case Float_(n) => evalFloat(n)
      case True => e.withValue(ScrubyTrueClass)
      case False => e.withValue(ScrubyFalseClass)
      case Nil_ => e.withValue(ScrubyFalseClass)
      case ScrubyObjectContainer(obj) => e.withValue(obj)
    }
  }
}
