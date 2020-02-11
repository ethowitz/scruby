package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object KlassDefEvaluator extends EvaluatorLike[KlassDefNode] {
  // Public members
  val eval: PartialFunction[KlassDefNode, Evaluation] = {
    case KlassDefNode(name, ts) => evalKlassDef(name, ts)
  }

  // Private members
  def evalKlassDef(name: Symbol, ts: List[AST]): Evaluation = for {
    Universe(_, _, prevSelf, prevScope) <- State.get[Universe]
    _ <- State.modify[Universe] { s =>
      s.copy(constants = s.constants.withConstant(name :: Nil, RubyClass(name)),
        self = RubyClass(name), scope = name :: Nil)
    }
    _ <- Evaluator.evalList(ts)
    _ <- State.modify[Universe] { s =>
      s.copy(constants = s.constants.withConstant(name :: Nil, s.self), self = prevSelf,
        scope = prevScope)
    }
  } yield RubySymbol(name)
}
