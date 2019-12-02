package org.sruby.evaluator

import cats.data.State
import org.sruby.parser.AST
import org.sruby.core.RubyObject

class RubyConstructor(val klass: Symbol, params: Seq[Symbol], ts: List[AST])
  extends RubyMethod(params, ts) {

  override def invoke(args: State[EvalState, Seq[RubyObject]]): State[EvalState, RubyObject] = for {
    _ <- State.modify[EvalState] { s =>
      val k = s.klasses.get(klass).getOrElse(throw new Exception(s"class ${klass} not found"))

      s.copy(self = k.newInstance)
    }
    _ <- super.invoke(args)
    newSelf <- State.inspect[EvalState, RubyObject](_.self)
  } yield newSelf
}

object RubyConstructor {
  def apply(klass: Symbol, params: Seq[Symbol], ts: List[AST]): RubyConstructor =
    new RubyConstructor(klass, params, ts)
}
