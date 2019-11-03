package com.ethowitz.sruby.parser

import com.ethowitz.sruby.core.RubyObject

// scalastyle:off cyclomatic.complexity
sealed class SyntaxTree {
  def withBoundVars(vars: Map[Symbol, RubyObject]): SyntaxTree = {
    def withBoundVars(t: SyntaxTree): SyntaxTree = t match {
      case KlassDefNode(name, ts) => KlassDefNode(name, ts map withBoundVars)
      case MethodDefNode(name, params, ts) => MethodDefNode(name, params, ts map withBoundVars)

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

final case class KlassDefNode(name: Symbol, statements: List[SyntaxTree]) extends SyntaxTree
final case class MethodDefNode(name: Symbol, params: List[Symbol], body: List[SyntaxTree])
  extends SyntaxTree

final case class LocalVarAssignmentNode(name: Symbol, value: SyntaxTree) extends SyntaxTree
final case class IvarAssignmentNode(name: Symbol, value: SyntaxTree) extends SyntaxTree
final case class RubyObjectContainerNode(obj: RubyObject) extends SyntaxTree

sealed trait InvocationNode extends SyntaxTree

final case class InvocationWithReceiverNode(
  receiver: SyntaxTree,
  message: Symbol,
  args: List[SyntaxTree]) extends InvocationNode

final case class InvocationWithImplicitReceiverNode(message: Symbol, args: List[SyntaxTree])
  extends InvocationNode

final case class NotSyntaxTreeNode(exp: SyntaxTree) extends SyntaxTree
final case class IfNode(
  predicate: SyntaxTree,
  yesBranch: List[SyntaxTree],
  noBranch: List[SyntaxTree]) extends SyntaxTree

final case class UnlessNode(predicate: SyntaxTree, body: List[SyntaxTree]) extends SyntaxTree
final case class IvarIdentifierNode(name: Symbol) extends SyntaxTree
final case class IdentifierNode(name: Symbol) extends SyntaxTree
final case class ConstantNode(name: Symbol) extends SyntaxTree
final case class ArrayNode(arr: Seq[SyntaxTree]) extends SyntaxTree
final case class HashNode(h: Map[SyntaxTree, SyntaxTree]) extends SyntaxTree
final case class StringNode(s: String) extends SyntaxTree
final case class SymbolNode(s: Symbol) extends SyntaxTree
final case class IntegerNode(i: Integer) extends SyntaxTree
final case class FloatNode(f: Float) extends SyntaxTree
case object TrueNode extends SyntaxTree
case object FalseNode extends SyntaxTree
case object NilNode extends SyntaxTree
