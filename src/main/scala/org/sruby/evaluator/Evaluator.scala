package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object Evaluator {
  // Public members
  type Evaluation = State[EvalState, RubyObject]

  def apply(ts: List[AST]): RubyObject = evalList(ts).run(EvalState.start).value._2

  // This can probably be made better using reflection
  def eval(t: AST): Evaluation = (KlassDefEvaluator.eval orElse
    MethodDefEvaluator.eval orElse
    InvocationEvaluator.eval orElse
    ConditionalEvaluator.eval orElse
    VariableEvaluator.eval orElse
    LiteralEvaluator.eval orElse
    ConstantEvaluator.eval).lift(t).getOrElse(throw new Exception("unimplemented"))

  def evalList(ts: List[AST]): Evaluation = {
    val initialState = State.pure[EvalState, RubyObject](RubyNilClass)

    ts.foldLeft(initialState) { (acc, t) => acc.flatMap { _ => eval(t) } }
  }
}
