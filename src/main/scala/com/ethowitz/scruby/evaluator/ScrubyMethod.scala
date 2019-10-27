package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.parser.SyntaxTree
import com.ethowitz.scruby.core.ScrubyObject

class ScrubyMethod(val params: Seq[Symbol], val ts: List[SyntaxTree])

object ScrubyMethod {
  def apply(arg: SyntaxTree): ScrubyMethod = ScrubyMethod(List(), List(arg))
  def apply(params: Seq[Symbol], ts: List[SyntaxTree]): ScrubyMethod = new ScrubyMethod(params, ts)

  def invoke(
    method: ScrubyMethod,
    args: Seq[ScrubyObject],
    evals: List[SyntaxTree] => EvaluationState
  ): EvaluationState = {
    val bindings: Map[Symbol, ScrubyObject] = (method.params zip args) toMap

    evals(method.ts.map(SyntaxTree.withBoundVars(_, bindings)))
  }
}
