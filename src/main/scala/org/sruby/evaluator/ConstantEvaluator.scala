package org.sruby.evaluator

import cats.data.State
import org.sruby.parser._

object ConstantEvaluator extends EvaluatorLike[ConstantNode] {
  // Public members
  val eval: PartialFunction[ConstantNode, Evaluator.Evaluation] = {
    case ConstantNode(name) => evalConstant(name)
  }

  private def evalConstant(name: Symbol): Evaluator.Evaluation = State { s =>
    s.klasses.get(name) match {
      case Some(value) => (s, value)
      case None => throw new Exception(s"unitialized constant ${name.toString}")
    }
  }
}
