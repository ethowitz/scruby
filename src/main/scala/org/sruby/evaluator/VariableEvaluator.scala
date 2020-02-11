package org.sruby.evaluator

import cats.data.State
import org.sruby.parser._

object VariableEvaluator extends EvaluatorLike[VariableNode] {
  // Public members
  val eval: PartialFunction[VariableNode, Evaluation] = {
    case LocalVarAssignmentNode(name, value) => evalLocalVarAssignment(name, value)
    case IvarAssignmentNode(name, value) => evalIvarAssignment(name, value)
    case IvarIdentifierNode(name) => evalIvarIdentifier(name)
  }

  // Private members
  private def evalIvarAssignment(name: Symbol, value: AST): Evaluation = for {
    result <- Evaluator.eval(value)
    _ <- State.modify[Universe] { s => s.copy(self = s.self.withInstanceVariable(name -> result)) }
  } yield result

  private def evalLocalVarAssignment(name: Symbol, value: AST): Evaluation = for {
    result <- Evaluator.eval(value)
    _ <- State.modify[Universe] { s => s.copy(localVars = s.localVars + (name -> result)) }
  } yield result

  private def evalIvarIdentifier(name: Symbol): Evaluation =
    State { s => (s, s.self.getInstanceVariable(name).getOrElse(RubyNil)) }
}
