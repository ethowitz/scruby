package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object VariableEvaluator extends EvaluatorLike {
  // Public members
  def eval: PartialFunction[AST, Evaluator.Evaluation] = {
    case LocalVarAssignmentNode(name, value) => evalLocalVarAssignment(name, value)
    case IvarAssignmentNode(name, value) => evalIvarAssignment(name, value)
    case IvarIdentifierNode(name) => evalIvarIdentifier(name)
    case IdentifierNode(name) => throw new Exception("attempted to eval an identifier") // eh
    case ConstantNode(name) => evalConstant(name)
  }

  private def evalIvarAssignment(name: Symbol, value: AST): Evaluator.Evaluation = for {
    result <- Evaluator.eval(value)
    _ <- State.modify[EvalState] { s => s.copy(self = s.self.withIvar(name -> result)) }
  } yield result

  private def evalLocalVarAssignment(name: Symbol, value: AST): Evaluator.Evaluation = for {
    result <- Evaluator.eval(value)
    _ <- State.modify[EvalState] { s => s.copy(localVars = s.localVars + (name -> result)) }
  } yield result

  private def evalConstant(name: Symbol): Evaluator.Evaluation = State { s =>
    s.klasses.get(name) match {
      case Some(value) => (s, value)
      case None => throw new Exception(s"unitialized constant ${name.toString}")
    }
  }

  private def evalIvarIdentifier(name: Symbol): Evaluator.Evaluation =
    State { s => (s, s.self.ivars.get(name).getOrElse(RubyNilClass)) }
}
