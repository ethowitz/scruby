package org.sruby.evaluator

import cats.data.State
import org.sruby.core.SRubyObject
import org.sruby.parser.AST

class RubyConstructor(val klass: List[Symbol], params: Seq[Symbol], ts: List[AST])
    extends RubyMethod(params, ts) {
  override def invoke(args: State[Universe, Seq[SRubyObject]]): Evaluation = for {
    _ <- State.modify[Universe] { s =>
      val k = s.constants.get(klass.toList).
        getOrElse(throw new Exception(s"class ${klass} not found"))

      s.copy(self = k.newInstance)
    }
    _ <- super.invoke(args)
    newSelf <- State.inspect[Universe, SRubyObject](_.self)
  } yield newSelf
}

object RubyConstructor {
  def apply(klass: List[Symbol], params: Seq[Symbol], ts: List[AST]): RubyConstructor =
    new RubyConstructor(klass, params, ts)
}
