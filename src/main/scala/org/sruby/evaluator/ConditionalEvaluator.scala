package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object ConditionalEvaluator extends EvaluatorLike[ConditionalNode] {
  // Public members
  val eval: PartialFunction[ConditionalNode, Evaluation] = {
    case IfNode(p, yes, no) => evalIf(p, yes, no)
    case UnlessNode(p, statements) => evalUnless(p, statements)
  }

  // Private members
  def evalIf(p: AST, yeses: List[AST], nos: List[AST]): Evaluation = for {
    predicateResult <- Evaluator.eval(p)
    result <- predicateResult match {
      case RubyFalse | RubyNil => Evaluator.evalList(nos)
      case _ => Evaluator.evalList(yeses)
    }
  } yield result

  def evalUnless(p: AST, ts: List[AST]): Evaluation = for {
    predicateResult <- Evaluator.eval(p)
    result <- predicateResult match {
      case RubyFalse| RubyNil=> Evaluator.evalList(ts)
      case _ => State.pure[Universe, SRubyObject](RubyNil)
    }
  } yield result
}
