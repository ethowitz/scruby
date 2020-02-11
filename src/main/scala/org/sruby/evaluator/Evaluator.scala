package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object Evaluator {
  // Public members
  def apply(trees: List[AST]): SRubyObject = evalList(trees).run(Universe.initial).value._2

  def eval(tree: AST): Evaluation = tree match {
    case t: KlassDefNode => KlassDefEvaluator.eval(t)
    case t: MethodDefNode => MethodDefEvaluator.eval(t)
    case t: InvocationNode => InvocationEvaluator.eval(t)
    case t: ConditionalNode => ConditionalEvaluator.eval(t)
    case t: VariableNode => VariableEvaluator.eval(t)
    case t: LiteralNode => LiteralEvaluator.eval(t)
    case t: ConstantNode => ConstantEvaluator.eval(t)
    case _ => throw new Exception("unimplemented")
  }

  def evalList(trees: List[AST]): Evaluation = {
    val initialState = State.pure[Universe, SRubyObject](RubyNil)

    trees.foldLeft(initialState) { (acc, t) => acc.flatMap { _ => eval(t) } }
  }
}
