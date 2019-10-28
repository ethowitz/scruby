package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.parser.SyntaxTree
import com.ethowitz.sruby.core.RubyObject

class RubyConstructor(params: Seq[Symbol], ts: List[SyntaxTree]) extends RubyMethod(params, ts)

object RubyConstructor {
  def apply(method: RubyMethod): RubyConstructor =
    new RubyConstructor(method.params, method.ts)

  def invoke(
    method: RubyMethod,
    args: Seq[RubyObject],
    evals: List[SyntaxTree] => EvaluationState
  ): EvaluationState = {
    val EvaluationState(v, ks, ls, s) = RubyMethod.invoke(method, args, evals)

    EvaluationState(
      s.getOrElse(throw new Exception("method invocation resulted in null self")),
      ks,
      ls,
    s)
  }
}
