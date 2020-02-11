package org.sruby.parser

import org.sruby.core.SRubyObject

// scalastyle:off cyclomatic.complexity
sealed class AST

final case class KlassDefNode(name: Symbol, statements: List[AST]) extends AST

sealed trait MethodDefNode extends AST
final case class InstanceMethodDefNode(name: Symbol, params: List[Symbol], body: List[AST])
  extends MethodDefNode

final case class KlassMethodDefNode(name: Symbol, params: List[Symbol], body: List[AST])
  extends MethodDefNode

sealed trait VariableNode extends AST
final case class IvarIdentifierNode(name: Symbol) extends VariableNode
final case class LocalVarAssignmentNode(name: Symbol, value: AST) extends VariableNode
final case class IvarAssignmentNode(name: Symbol, value: AST) extends VariableNode

final case class SRubyObjectContainerNode(obj: SRubyObject) extends AST

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

final case class IdentifierNode(name: Symbol) extends AST
final case class ConstantNode(name: Symbol) extends AST

sealed trait LiteralNode extends AST
final case class ArrayNode(arr: Seq[AST]) extends LiteralNode
final case class HashNode(h: Map[AST, AST]) extends LiteralNode
final case class StringNode(s: String) extends LiteralNode
final case class SymbolNode(s: Symbol) extends LiteralNode
final case class IntegerNode(i: Integer) extends LiteralNode
final case class FloatNode(f: Float) extends LiteralNode
case object TrueNode extends LiteralNode
case object FalseNode extends LiteralNode
case object NilNode extends LiteralNode
case object SelfNode extends LiteralNode
