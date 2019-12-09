package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object VariableEvaluator extends EvaluatorLike[VariableNode] {
  // Public members
  val eval: PartialFunction[VariableNode, Evaluator.Evaluation] = {
    case LocalVarAssignmentNode(name, value) => evalLocalVarAssignment(name, value)
    case IvarAssignmentNode(name, value) => evalIvarAssignment(name, value)
    case IvarIdentifierNode(name) => evalIvarIdentifier(name)
  }

  // Private members
  private def evalIvarAssignment(name: Symbol, value: AST): Evaluator.Evaluation = for {
    result <- Evaluator.eval(value)
    _ <- State.modify[EvalState] { s => s.copy(self = s.self.withIvar(name -> result)) }
  } yield result

  private def evalLocalVarAssignment(name: Symbol, value: AST): Evaluator.Evaluation = for {
    result <- Evaluator.eval(value)
    _ <- State.modify[EvalState] { s => s.copy(localVars = s.localVars + (name -> result)) }
  } yield result

  private def evalIvarIdentifier(name: Symbol): Evaluator.Evaluation =
    State { s => (s, s.self.ivars.get(name).getOrElse(RubyNilClass)) }
}
