package org.sruby.parser

import org.sruby.core.RubyObject

// scalastyle:off cyclomatic.complexity
sealed class AST {
  def withBoundVars(vars: Map[Symbol, RubyObject]): AST = {
    def withBoundVars(t: AST): AST = t match {
      case KlassDefNode(name, ts) => KlassDefNode(name, ts map withBoundVars)
      case InstanceMethodDefNode(name, params, ts) =>
        InstanceMethodDefNode(name, params, ts map withBoundVars)

      case KlassMethodDefNode(name, params, ts) =>
        KlassMethodDefNode(name, params, ts map withBoundVars)

      case InvocationWithReceiverNode(IdentifierNode(recvr), msg, ts) => vars get recvr match {
        case Some(arg) =>
          InvocationWithReceiverNode(RubyObjectContainerNode(arg), msg, ts map withBoundVars)
        case None => InvocationWithReceiverNode(IdentifierNode(recvr), msg, ts map withBoundVars)
      }

      case InvocationWithReceiverNode(recvr, msg, ts) =>
        InvocationWithReceiverNode(withBoundVars(recvr), msg, ts map withBoundVars)

      case invocation @ InvocationWithImplicitReceiverNode(msg, Nil) => vars get msg match {
        case Some(arg) => RubyObjectContainerNode(arg)
        case None => invocation
      }

      case InvocationWithImplicitReceiverNode(msg, ts) =>
        InvocationWithImplicitReceiverNode(msg, ts map withBoundVars)

      case IfNode(p, yes, no) =>
        IfNode(withBoundVars(p), yes map withBoundVars, no map withBoundVars)

      case UnlessNode(p, ts) => UnlessNode(withBoundVars(p), ts map withBoundVars)

      case node @ IvarAssignmentNode(name, InvocationWithImplicitReceiverNode(value, Nil)) =>
        vars get value match {
          case Some(arg) => IvarAssignmentNode(name, RubyObjectContainerNode(arg))
          case None => node
        }

      case node @ LocalVarAssignmentNode(name, InvocationWithImplicitReceiverNode(value, Nil)) =>
        vars get value match {
          case Some(arg) => LocalVarAssignmentNode(name, RubyObjectContainerNode(arg))
          case None => node
        }

      // Removing the default case results in extremely long compilation times
      case default => default
    }

    withBoundVars(this)
  }
}

final case class KlassDefNode(name: Symbol, statements: List[AST]) extends AST

sealed trait MethodDefNode extends AST
final case class InstanceMethodDefNode(name: Symbol, params: List[Symbol], body: List[AST])
  extends MethodDefNode

final case class KlassMethodDefNode(name: Symbol, params: List[Symbol], body: List[AST])
  extends MethodDefNode

final case class LocalVarAssignmentNode(name: Symbol, value: AST) extends AST
final case class IvarAssignmentNode(name: Symbol, value: AST) extends AST
final case class RubyObjectContainerNode(obj: RubyObject) extends AST

sealed trait InvocationNode extends AST
final case class InvocationWithReceiverNode(receiver: AST, message: Symbol, args: List[AST])
  extends InvocationNode

final case class InvocationWithImplicitReceiverNode(message: Symbol, args: List[AST])
  extends InvocationNode

final case class NotASTNode(exp: AST) extends AST

sealed trait ConditionalNode extends AST
final case class IfNode(predicate: AST, yesBranch: List[AST], noBranch: List[AST])
  extends ConditionalNode

final case class UnlessNode(predicate: AST, body: List[AST])
  extends ConditionalNode

final case class IvarIdentifierNode(name: Symbol) extends AST
final case class IdentifierNode(name: Symbol) extends AST
final case class ConstantNode(name: Symbol) extends AST
final case class ArrayNode(arr: Seq[AST]) extends AST
final case class HashNode(h: Map[AST, AST]) extends AST
final case class StringNode(s: String) extends AST
final case class SymbolNode(s: Symbol) extends AST
final case class IntegerNode(i: Integer) extends AST
final case class FloatNode(f: Float) extends AST
case object TrueNode extends AST
case object FalseNode extends AST
case object NilNode extends AST
case object SelfNode extends AST
