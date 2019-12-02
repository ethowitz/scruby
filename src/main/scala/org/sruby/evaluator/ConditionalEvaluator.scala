package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object ConditionalEvaluator extends EvaluatorLike {
  // Public members
  def eval: PartialFunction[AST, Evaluator.Evaluation] = {
    case IfNode(p, yes, no) => evalIf(p, yes, no)
    case UnlessNode(p, statements) => evalUnless(p, statements)
  }

  // Private members
  def evalIf(p: AST, yeses: List[AST], nos: List[AST]): Evaluator.Evaluation = for {
    predicateResult <- Evaluator.eval(p)
    result <- predicateResult match {
      case RubyFalseClass | RubyNilClass => Evaluator.evalList(nos)
      case _ => Evaluator.evalList(yeses)
    }
  } yield result

  def evalUnless(p: AST, ts: List[AST]): Evaluator.Evaluation = for {
    predicateResult <- Evaluator.eval(p)
    result <- predicateResult match {
      case RubyFalseClass | RubyNilClass => Evaluator.evalList(ts)
      case _ => State.pure[EvalState, RubyObject](RubyNilClass)
    }
  } yield result
}
