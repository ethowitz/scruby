package com.ethowitz.scruby.parser

import com.ethowitz.scruby.core.ScrubyObject

sealed trait SyntaxTree {
  def withBoundVars(tree: SyntaxTree, vars: Map[Symbol, ScrubyObject]): SyntaxTree = {
    def withBoundVars(t: SyntaxTree): SyntaxTree = t match {
      case KlassDef(name, ts) => KlassDef(name, ts map withBoundVars)
      case MethodDef(name, params, ts) => MethodDef(name, params, ts map withBoundVars)
      case Invocation(Some(Identifier(recvr)), msg, ts) => vars get recvr match {
        case Some(arg) => Invocation(Some(ScrubyObjectContainer(arg)), msg, ts map withBoundVars)
        case None => Invocation(Some(Identifier(recvr)), msg, ts map withBoundVars)
      }
      case Invocation(Some(recvr), msg, ts) =>
        Invocation(Some(withBoundVars(recvr)), msg, ts map withBoundVars)
      case If(p, yes, no) =>
        If(withBoundVars(p), yes map withBoundVars, no map withBoundVars)
      case Unless(p, ts) => Unless(withBoundVars(p), ts map withBoundVars)
      case t => t
    }

    withBoundVars(tree)
  }
}

case object SyntaxTree extends SyntaxTree

case class KlassDef(name: Symbol, statements: List[SyntaxTree]) extends SyntaxTree
case class MethodDef(name: Symbol, params: List[Symbol], body: List[SyntaxTree])
  extends SyntaxTree
case class Assignment(name: Symbol, value: SyntaxTree) extends SyntaxTree
case class ScrubyObjectContainer(obj: ScrubyObject) extends SyntaxTree
case class Invocation(receiver: Option[SyntaxTree], message: Symbol, args: List[SyntaxTree])
  extends SyntaxTree
case class NotSyntaxTree(exp: SyntaxTree) extends SyntaxTree
case class If(predicate: SyntaxTree, yesBranch: List[SyntaxTree], noBranch: List[SyntaxTree])
  extends SyntaxTree
case class Unless(predicate: SyntaxTree, body: List[SyntaxTree]) extends SyntaxTree
case class Identifier(name: Symbol) extends SyntaxTree
case class Array_(arr: Seq[Any]) extends SyntaxTree
case class Hash(h: Map[Any, Any]) extends SyntaxTree
case class String_(s: String) extends SyntaxTree
case class Symbol_(s: Symbol) extends SyntaxTree
case class Integer_(n: Integer) extends SyntaxTree
case class Float_(n: Float) extends SyntaxTree
case object True extends SyntaxTree
case object False extends SyntaxTree
case object Nil_ extends SyntaxTree
