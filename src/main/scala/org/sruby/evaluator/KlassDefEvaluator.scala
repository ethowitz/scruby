package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object KlassDefEvaluator extends EvaluatorLike[KlassDefNode] {
  // Public members
  val eval: PartialFunction[KlassDefNode, Evaluator.Evaluation] = {
    case KlassDefNode(name, ts) => evalKlassDef(name, ts)
  }

  // Private members
  def evalKlassDef(name: Symbol, ts: List[AST]): Evaluator.Evaluation = for {
    // in the future we will do RubyConstant#pop or something like that to get the old scope back
    EvalState(_, _, prevSelf, prevScope) <- State.get[EvalState]
    _ <- State.modify[EvalState] { s =>
      s.copy(klasses = s.klasses + (name -> RubyKlass(name)), self = RubyKlass(name),
        scope = name)
    }
    _ <- Evaluator.evalList(ts)
    _ <- State.modify[EvalState] { s =>
      s.copy(klasses = s.klasses + (name -> s.self), self = prevSelf, scope = prevScope)
    }
  } yield RubySymbol(name)
}
