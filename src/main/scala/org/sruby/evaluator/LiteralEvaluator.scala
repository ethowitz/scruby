package org.sruby.evaluator

import cats.data.State
import org.sruby.parser._

object LiteralEvaluator extends EvaluatorLike[LiteralNode] {
  val eval: PartialFunction[LiteralNode, Evaluation] = {
    //case StringNode(s) => e.withValue(RubyString(s))
    //case SymbolNode(s) => e.withValue(RubySymbol(s))
    //case Integer_(n) => evalInteger(n)
    //case Float_(n) => evalFloat(n)
    case SelfNode => State.get[Universe].map(_.self)
    case TrueNode => State.pure(RubyTrue)
    case FalseNode => State.pure(RubyFalse)
    case NilNode => State.pure(RubyNil)
    //case RubyObjectContainerNode(obj) => State.pure(obj)
  }
}
