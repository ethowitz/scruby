package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.parser.SyntaxTree
import com.ethowitz.sruby.core.RubyObject

class RubyMethod(val params: Seq[Symbol], val ts: List[SyntaxTree]) {
  def invoke(args: Seq[RubyObject], evals: List[SyntaxTree] => EvaluationState): EvaluationState = {
    args.length == params.length match {
      case true =>
        val bindings: Map[Symbol, RubyObject] = (params zip args).toMap

        evals(ts.map(_.withBoundVars(bindings)))
      case false =>
        val errorMessage =
          s"wrong number of arguments (given ${args.length}, expected ${params.length})"

        throw new Exception(errorMessage)
    }
  }
}

object RubyMethod {
  def apply(arg: SyntaxTree): RubyMethod = RubyMethod(List(), List(arg))
  def apply(params: Seq[Symbol], ts: List[SyntaxTree]): RubyMethod = new RubyMethod(params, ts)
}
