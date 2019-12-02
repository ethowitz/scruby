package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object LiteralEvaluator extends EvaluatorLike {
  def eval: PartialFunction[AST, Evaluator.Evaluation] = {
    //case StringNode(s) => e.withValue(RubyString(s))
    //case SymbolNode(s) => e.withValue(RubySymbol(s))
    //case Integer_(n) => evalInteger(n)
    //case Float_(n) => evalFloat(n)
    case SelfNode => State.get[EvalState].map(_.self)
    case TrueNode => State.pure(RubyTrueClass)
    case FalseNode => State.pure(RubyFalseClass)
    case NilNode => State.pure(RubyNilClass)
    case RubyObjectContainerNode(obj) => State.pure(obj)
  }
}
