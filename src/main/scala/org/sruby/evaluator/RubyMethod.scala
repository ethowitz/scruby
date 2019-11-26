package org.sruby.evaluator

import cats.data.State
import org.sruby.parser.AST
import org.sruby.core.RubyObject

class RubyMethod(val params: Seq[Symbol], val ts: List[AST]) {
  def invoke(
    args: State[EvalState, Seq[RubyObject]],
    evals: List[AST] => State[EvalState, RubyObject]
  ): State[EvalState, RubyObject] = args.flatMap { evaldArgs =>
    evaldArgs.length == params.length match {
      case true =>
        val bindings = (params zip evaldArgs).toMap
        val tsWithBoundVars = ts.map(_.withBoundVars(bindings))

        for { result <- evals(tsWithBoundVars) } yield result
      case false =>
        val errorMessage =
          s"wrong number of arguments (given ${evaldArgs.length}, expected ${params.length})"

        throw new Exception(errorMessage)
    }
  }
}

object RubyMethod {
  def apply(arg: AST): RubyMethod = RubyMethod(List(), List(arg))
  def apply(params: Seq[Symbol], ts: List[AST]): RubyMethod = new RubyMethod(params, ts)
}
