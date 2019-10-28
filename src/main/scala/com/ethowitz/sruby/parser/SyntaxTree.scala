package com.ethowitz.sruby.parser

import com.ethowitz.sruby.core.RubyObject

sealed trait SyntaxTree {
  def withBoundVars(tree: SyntaxTree, vars: Map[Symbol, RubyObject]): SyntaxTree = {
    def withBoundVars(t: SyntaxTree): SyntaxTree = t match {
      case KlassDefNode(name, ts) => KlassDefNode(name, ts map withBoundVars)
      case MethodDefNode(name, params, ts) => MethodDefNode(name, params, ts map withBoundVars)
      case InvocationNode(Some(IdentifierNode(recvr)), msg, ts) => vars get recvr match {
        case Some(arg) =>
          InvocationNode(Some(RubyObjectContainerNode(arg)), msg, ts map withBoundVars)
        case None => InvocationNode(Some(IdentifierNode(recvr)), msg, ts map withBoundVars)
      }
      case InvocationNode(Some(recvr), msg, ts) =>
        InvocationNode(Some(withBoundVars(recvr)), msg, ts map withBoundVars)
      case IfNode(p, yes, no) =>
        IfNode(withBoundVars(p), yes map withBoundVars, no map withBoundVars)
      case UnlessNode(p, ts) => UnlessNode(withBoundVars(p), ts map withBoundVars)
      case t => t
    }

    withBoundVars(tree)
  }
}

case object SyntaxTree extends SyntaxTree

final case class KlassDefNode(name: Symbol, statements: List[SyntaxTree]) extends SyntaxTree
final case class MethodDefNode(name: Symbol, params: List[Symbol], body: List[SyntaxTree])
  extends SyntaxTree

final case class LocalVarAssignmentNode(name: Symbol, value: SyntaxTree) extends SyntaxTree
final case class IvarAssignmentNode(name: Symbol, value: SyntaxTree) extends SyntaxTree
final case class RubyObjectContainerNode(obj: RubyObject) extends SyntaxTree
final case class InvocationNode(
  receiver: Option[SyntaxTree],
  message: Symbol,
  args: List[SyntaxTree]) extends SyntaxTree

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
