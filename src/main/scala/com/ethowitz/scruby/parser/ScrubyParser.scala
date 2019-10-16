package com.ethowitz.scruby.parser

import com.ethowitz.scruby.exceptions.ParserError
import com.ethowitz.scruby.lexer._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

object ScrubyParser extends Parsers {
  override type Elem = Token

  def apply(tokens: Seq[Token]): Either[ParserError, List[SyntaxTree]] = {
    val reader = new TokenReader(tokens)

    phrase(repsep(definition | expression, Separator))(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  val keywords = List("def", "end", "true", "false", "class", "if", "elsif", "unless", "else",
    "nil")

  def conditional: Parser[SyntaxTree] = iff | unless

  def definition: Parser[SyntaxTree] = klassDef | methodDef

  def elsif: Parser[List[SyntaxTree] => If] = {
    (elsifToken ~> expression ~ Separator ~ sequence(expression)) ^^ {
      case predicate ~ _ ~ yesBranch => If.curried(predicate)(yesBranch)
    }
  }

  def assignment: Parser[SyntaxTree] = (idsWithoutKeywords ~ Assigner ~ expression) ^^ {
    case Identifier(name) ~ _ ~ value => Assignment(name, value)
  }

  def expression: Parser[SyntaxTree] = assignment | conditional | invocation |
    invocationWithoutReceiver | literal

  def sequence[A](parser: Parser[A]): Parser[List[A]] = {
    (repsep(parser, Separator) <~ Separator).? ^^ {
      case Some(exps) => exps
      case None => List[A]()
    }
  }

  def defToken: Parser[Token] = elem(IdentifierToken("def"))
  def endToken: Parser[Token] = elem(IdentifierToken("end"))
  def klassToken: Parser[Token] = elem(IdentifierToken("class"))
  def ifToken: Parser[Token] = elem(IdentifierToken("if"))
  def elsifToken: Parser[Token] = elem(IdentifierToken("elsif"))
  def elseToken: Parser[Token] = elem(IdentifierToken("else"))
  def unlessToken: Parser[Token] = elem(IdentifierToken("unless"))

  def idsWithoutKeywords: Parser[Identifier] = {
    accept("identifier", {
      case IdentifierToken(name) if !(keywords contains name) => Identifier(Symbol(name))
    })
  }

  def constant: Parser[Constant] = {
    accept("constant", { case ConstantToken(name) => Constant(Symbol(name)) })
  }

  def identifier: Parser[Identifier] = {
    accept("identifier", { case IdentifierToken(name) => Identifier(Symbol(name)) })
  }

  def ivarIdentifier: Parser[IvarIdentifier] = (IvarPrefix ~> identifier) ^^ {
    case IdentifierToken(name) => IvarIdentifier(name)
  }

  def iff: Parser[If] = {
    (ifToken ~> expression ~ Separator ~ sequence(expression) ~ rep(elsif) ~
      (elseToken ~ Separator ~ sequence(expression)).? ~ endToken) ^^ {
      case predicate ~ _ ~ yesBranch ~ elsifs ~ Some(_ ~ _~ noBranch) ~ _ => {
        If(predicate, yesBranch, elsifs.foldRight(noBranch) { (acc, ifExp) => List(acc(ifExp)) })
      }
      case predicate ~ _ ~ yesBranch ~ elsifs ~ None ~ _ => {
        If(predicate, yesBranch, elsifs.foldRight(List[If]()) { (acc, ifExp) => List(acc(ifExp)) })
      }
    }
  }

  def invocationWithoutReceiver: Parser[Invocation] = {
    (message(idsWithoutKeywords)) ^^ {
      case invWithoutReceiver => invWithoutReceiver(None)
    }
  }

  def invocation: Parser[Invocation] = {
    (leftmostReceiver ~ Period ~ message(identifier)) ^^ {
      case recv ~ _ ~ inv => inv(Some(recv))
    }
  }

  def klassDef: Parser[KlassDef] = {
    (klassToken ~> idsWithoutKeywords ~ Separator ~ sequence(definition) ~ endToken) ^^ {
      case Identifier(name) ~ _ ~ expressions ~  _ => KlassDef(name, expressions)
    }
  }

  def literal: Parser[SyntaxTree] = {
    accept("literal", {
      case StringLiteral(s) => String_(s)
      case SymbolLiteral(s) => Symbol_(Symbol(s))
      case IntegerLiteral(n) => Integer_(n)
      case FloatLiteral(n) => Float_(n)
      case IdentifierToken("true") => True
      case IdentifierToken("false") => False
      case IdentifierToken("nil") => Nil_
    })
  }

  def message(id: Parser[Identifier]): Parser[Option[SyntaxTree] => Invocation] =
    messageParens(id) | messageNoParens(id)

  def messageNoParens(id: Parser[Identifier]): Parser[Option[SyntaxTree] => Invocation] = {
    (id ~ repsep(expression, Comma)) ^^ {
      case Identifier(name) ~ args => Invocation.curried(_: Option[SyntaxTree])(name)(args)
    }
  }

  def messageParens(id: Parser[Identifier]): Parser[Option[SyntaxTree] => Invocation] = {
    (id ~ OpeningParenthesis ~ repsep(expression, Comma) ~ ClosingParenthesis) ^^ {
      case Identifier(name) ~ _ ~ args ~ _ => Invocation.curried(_: Option[SyntaxTree])(name)(args)
    }
  }

  def methodDef: Parser[MethodDef] = {
    (defToken ~> identifier ~ (OpeningParenthesis ~ repsep(idsWithoutKeywords, Comma) ~
        ClosingParenthesis).? ~ Separator ~ sequence(expression) ~ endToken) ^^ {
      case Identifier(name) ~ Some(_ ~ params ~ _) ~ _ ~ expressions ~ _ =>
        MethodDef(name, params.map(_.name), expressions)
      case Identifier(name) ~ None ~ _ ~ expressions ~ _ =>
        MethodDef(name, List[Symbol](), expressions)
    }
  }

  def leftmostReceiver: Parser[SyntaxTree] = {
    ((literal | conditional | idsWithoutKeywords) ~
        (Period ~ message(identifier) ~ receiver).?) ^^ {
      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
    }
  }

  def receiver: Parser[SyntaxTree] = {
    ((literal | conditional | identifier) ~ (Period ~ message(identifier) ~ receiver).?) ^^ {
      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
    }
  }

  def unless: Parser[Unless] = {
    (unlessToken ~ expression ~ Separator ~ sequence(expression) ~ endToken) ^^ {
      case _ ~ predicate ~ _ ~ expressions ~ _ => Unless(predicate, expressions)
    }
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}
