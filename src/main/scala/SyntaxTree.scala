package scruby

sealed trait SyntaxTree {
  def withBoundVars(tree: SyntaxTree, map: Map[String, ScrubyObjectContainer]): SyntaxTree = {
    def withBoundVars(t: SyntaxTree): SyntaxTree = t match {
      case KlassDef(name, ts) => KlassDef(name, ts.map(withBoundVars(_)))
      case MethodDef(name, params, ts) => MethodDef(name, params, ts.map(withBoundVars(_)))
      case Invocation(recvr, msg, ts) =>
        Invocation(withBoundVars(recvr), msg, ts.map(withBoundVars(_)))
      case If(p, yes, no) =>
        If(withBoundVars(t), yes.map(withBoundVars(_)), no.map(withBoundVars(_)))
      case Unless(p, ts) => Unless(withBoundVars(p), ts.map(withBoundVars(_)))
      case Identifier(name) => map(name)
      case lit => lit
    }

    withBoundVars(tree)
  }
}

case object SyntaxTree extends SyntaxTree

case class KlassDef(name: String, statements: List[SyntaxTree]) extends SyntaxTree
case class MethodDef(name: String, params: List[String], body: List[SyntaxTree]) extends SyntaxTree
case class ScrubyObjectContainer(obj: ScrubyObject) extends SyntaxTree
case class Invocation(receiver: SyntaxTree, message: String, args: List[SyntaxTree]) extends SyntaxTree
case class NotSyntaxTree(exp: SyntaxTree) extends SyntaxTree
case class If(predicate: SyntaxTree, yesBranch: List[SyntaxTree], noBranch: List[SyntaxTree]) extends SyntaxTree
case class Unless(predicate: SyntaxTree, body: List[SyntaxTree]) extends SyntaxTree
case class Identifier(name: String) extends SyntaxTree
case class Array_(arr: Seq[Any]) extends SyntaxTree
case class Hash(h: Map[Any, Any]) extends SyntaxTree
case class String_(s: String) extends SyntaxTree
case class Symbol_(s: Symbol) extends SyntaxTree
case class Integer_(n: Integer) extends SyntaxTree
case class Float_(n: Float) extends SyntaxTree
case object True extends SyntaxTree
case object False extends SyntaxTree
case object Nil_ extends SyntaxTree
