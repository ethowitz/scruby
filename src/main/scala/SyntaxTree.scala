package scruby

sealed trait SyntaxTree

sealed trait Definition extends SyntaxTree
case class KlassDef(methods: List[MethodDef]) extends Definition
case class MethodDef(name: String, params: List[String], body: List[Expression]) extends Definition

sealed trait Expression extends SyntaxTree
case class Invocation(receiver: Expression, message: String, args: List[Expression]) extends Expression
case class NotExpression(exp: Expression) extends Expression
case class If(predicate: Expression, positiveBranch: List[Expression], negativeBranch: List[Expression]) extends Expression
case class Unless(predicate: Expression, body: List[Expression]) extends Expression

sealed trait Literal extends Expression
case class Array_(arr: Seq[Any]) extends Literal
case class Hash(h: Map[Any, Any]) extends Literal
case class String_(s: String) extends Literal
case class Symbol(s: String) extends Literal
case class Integer_(n: Integer) extends Literal
case class Float_(n: Float) extends Literal
case object True extends Literal
case object False extends Literal
case object Nil_ extends Literal
