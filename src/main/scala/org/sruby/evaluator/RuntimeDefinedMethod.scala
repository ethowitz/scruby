package org.sruby.evaluator

import cats.data.State
import org.sruby.core.SRubyObject
import org.sruby.parser.AST

final case class RuntimeDefinedMethod(val params: Seq[Symbol], val ts: List[AST])
    extends RubyMethod {
  def invoke(args: State[Universe, Seq[SRubyObject]]): State[Universe, SRubyObject] =
    args.flatMap { evaluatedArgs =>
      if (evaluatedArgs.length == params.length) {
        val argMap = (params zip evaluatedArgs).toMap

        State.modify[Universe] { u => u.copy(localVars = u.localVars ++ argMap) }.
          flatMap { _ => Evaluator.evalList(ts) }
      } else {
        val errorMessage =
          s"wrong number of arguments (given ${evaluatedArgs.length}, expected ${params.length})"

        throw new Exception(errorMessage)
      }
    }
}
