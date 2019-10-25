package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.parser.SyntaxTree
import com.ethowitz.scruby.core.ScrubyObject

class ScrubyConstructor(params: Seq[Symbol], ts: List[SyntaxTree]) extends ScrubyMethod(params, ts)

object ScrubyConstructor {
  def apply(method: ScrubyMethod): ScrubyConstructor = new ScrubyConstructor(method.params, method.ts)

  def invoke(method: ScrubyMethod, args: Seq[ScrubyObject], evals: List[SyntaxTree] => EvaluationState): EvaluationState = {
    val EvaluationState(v, ks, ls, s) = ScrubyMethod.invoke(method, args, evals)

    EvaluationState(s.get, ks, ls, s)
  }
}
