package scruby

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object ScrubyParser extends Parsers {
  override type Elem = Token

  def conditional: Parser[Expression] = iff | unless

  def expression: Parser[Expression] = invocation | literal | conditional

  def iff: Parser[If] = {
    (IfToken ~> expression ~ rep(expression) ~ rep(elsif) ~ (Else ~> rep(expression)).? <~ End) ^^ {
      case predicate ~ yesBranch ~ elsifs ~ Some(noBranch) => {
        If(predicate, yesBranch, elsifs.foldLeft(noBranch) { (acc, ifExp) => List(ifExp(acc)) })
      }
      case predicate ~ yesBranch ~ elsifs ~ None => {
        If(predicate, yesBranch, elsifs.foldLeft(List[If]()) { (acc, ifExp) => List(ifExp(acc)) })
      }
    }
  }

  private def elsif: Parser[List[Expression] => If] = {
    (Elsif ~> expression ~ (expression).*) ^^ {
      case predicate ~ expressions => If.curried(predicate)(expressions)
    }
  }

  def unless: Parser[Unless] = {
    (UnlessToken ~> expression ~ rep(expression) <~ End) ^^ {
      case predicate ~ expressions => Unless(predicate, expressions)
    }
  }

  def invocation: Parser[Invocation] = explicitInvocation | implicitInvocation

  private def explicitInvocation: Parser[Invocation] = {
    (expression ~ Period ~ identifier ~ repsep(expression, Comma)) ^^ {
      case expression ~ _ ~ name ~ args => Invocation(expression, name, args)
    }
  }

  private def implicitInvocation: Parser[Invocation] = {
    (identifier ~ repsep(expression, Comma)) ^^ {
      case name ~ args => Invocation(null, name, args)
    }
  }

  def klassDef: Parser[KlassDef] = {
    (Klass ~> identifier ~ rep(methodDef) <~ End) ^^ {
      case identifier ~ methodDefs => KlassDef(identifier, methodDefs)
    }
  }

  def methodDef: Parser[MethodDef] = {
    (Def ~> identifier ~ rep1sep(identifier, Comma) ~ rep1(expression) <~ End) ^^ {
      case identifier ~ params ~ expressions => MethodDef(identifier, params, expressions)
    }
  }

  def identifier: Parser[String] = {
    accept("identifier", { case id @ Identifier(name) => name })
  }

  def literal: Parser[Literal] = {
    accept("literal", {
      case StringLiteral(s) => String_(s)
      case SymbolLiteral(s) => Symbol_(s)
      case IntegerLiteral(n) => Integer_(n)
      case FloatLiteral(n) => Float_(n)
      case TrueLiteral => True
      case FalseLiteral => False
      case NilLiteral => Nil_
    })
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

