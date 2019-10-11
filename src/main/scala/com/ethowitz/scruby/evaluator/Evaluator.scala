package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.parser._
import com.ethowitz.scruby.core._
import scala.annotation.tailrec

object Evaluator {
  def apply(ts: List[SyntaxTree]) = evals(ts, EvaluationState.start)

  def evals(ts: List[SyntaxTree], e: EvaluationState): EvaluationState = ts match {
    case Nil => e
    case t :: ts => evals(ts, eval(t, e))
  }

  def eval(t: SyntaxTree, e: EvaluationState): EvaluationState = {
    def evalAssignment(name: Symbol, value: SyntaxTree): EvaluationState = {
      val EvaluationState(v, ks, ls, self) = eval(value, e)

      // take intersection of inner var map and outer var map upon exiting a block? do we need this? maybe for ivars
      EvaluationState(v, ks, ls + (name -> v), self)
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

    def evalInvocation(recvr: Option[SyntaxTree], msg: Symbol, args: List[SyntaxTree]): EvaluationState = {
      val receivingState = recvr match {
        case Some(r) => eval(r, e)
        case None => e.self match {
          case Some(s) => e.withValue(s)
          case None => throw new Exception("cannot call method with no receiver")
        }
      }
      val method = receivingState.value.methods get msg

      args match {
        case Nil => ScrubyMethod.invoke(method, Nil, ts => evals(ts, receivingState.withSelf(receivingState.value)))
        case ts => 
          val evaldArgs = args.scanLeft(receivingState) { (acc, t) => eval(t, acc) }

          ScrubyMethod.invoke(method, evaldArgs.map(_.value), ts => evals(ts, evaldArgs.last.withSelf(receivingState.value)))
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
      case Assignment(name, value) => evalAssignment(name, value)
      case KlassDef(name, ts) => evalKlassDef(name, ts)
      case MethodDef(name, params, ts) => evalMethodDef(name, params, ts)
      case Invocation(recvr, msg, args) => evalInvocation(recvr, msg, args)
      case If(p, yes, no) => evalIf(p, yes, no)
      case Unless(p, statements) => evalUnless(p, statements)
      case Identifier(klass) => e.withValue(e.klasses get klass)
      case String_(s) => e.withValue(ScrubyString(s))
      case Symbol_(s) => e.withValue(ScrubySymbol(s)) // fake implementation of Symbol
      //case Integer_(n) => evalInteger(n)
      //case Float_(n) => evalFloat(n)
      case True => e.withValue(ScrubyTrueClass)
      case False => e.withValue(ScrubyFalseClass)
      case Nil_ => e.withValue(ScrubyFalseClass)
      case ScrubyObjectContainer(obj) => e.withValue(obj)
    }
  }
}
