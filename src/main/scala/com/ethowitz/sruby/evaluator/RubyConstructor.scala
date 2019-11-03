package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.parser.SyntaxTree
import com.ethowitz.sruby.core.RubyObject

class RubyConstructor(params: Seq[Symbol], ts: List[SyntaxTree]) extends RubyMethod(params, ts) {
  override def invoke(
    args: Seq[RubyObject],
    evals: List[SyntaxTree] => EvaluationState
  ): EvaluationState = {
    val EvaluationState(v, ks, ls, s) = super.invoke(args, evals)

    EvaluationState(s, ks, ls, s)
  }
}

object RubyConstructor {
  def apply(method: RubyMethod): RubyConstructor = new RubyConstructor(method.params, method.ts)
}
