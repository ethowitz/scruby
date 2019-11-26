package org.sruby.evaluator

import cats.data.State
import org.sruby.parser.AST
import org.sruby.core.RubyObject

class RubyConstructor(params: Seq[Symbol], ts: List[AST]) extends RubyMethod(params, ts) {
  override def invoke(
    args: State[EvalState, Seq[RubyObject]],
    evals: List[AST] => State[EvalState, RubyObject]
  ): State[EvalState, RubyObject] = for {
    _ <- super.invoke(args, evals)
    EvalState(_, _, self) <- State.get
  } yield self
}

object RubyConstructor {
  def apply(method: RubyMethod): RubyConstructor = new RubyConstructor(method.params, method.ts)
}
