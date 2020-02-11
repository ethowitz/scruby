package org.sruby.evaluator

import cats.data.State
import org.sruby.parser._

object ConstantEvaluator extends EvaluatorLike[ConstantNode] {
  // Public members
  val eval: PartialFunction[ConstantNode, Evaluation] = {
    case ConstantNode(name) => evalConstant(name)
  }

  private def evalConstant(name: Symbol): Evaluation = State { u =>
    u.constants.get(name :: Nil) match {
      case Some(value) => (u, value)
      case None => throw new Exception(s"unitialized constant ${name.toString}")
    }
  }
}
