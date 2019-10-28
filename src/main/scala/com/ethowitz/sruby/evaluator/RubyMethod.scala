package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.parser.SyntaxTree
import com.ethowitz.sruby.core.RubyObject

class RubyMethod(val params: Seq[Symbol], val ts: List[SyntaxTree])

object RubyMethod {
  def apply(arg: SyntaxTree): RubyMethod = RubyMethod(List(), List(arg))
  def apply(params: Seq[Symbol], ts: List[SyntaxTree]): RubyMethod = new RubyMethod(params, ts)

  def invoke(
    method: RubyMethod,
    args: Seq[RubyObject],
    evals: List[SyntaxTree] => EvaluationState
  ): EvaluationState = {
    val bindings: Map[Symbol, RubyObject] = (method.params zip args).toMap

    evals(method.ts.map(SyntaxTree.withBoundVars(_, bindings)))
  }
}
