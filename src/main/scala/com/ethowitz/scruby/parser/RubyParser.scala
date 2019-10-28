package com.ethowitz.scruby.parser

import com.ethowitz.scruby.exceptions.ParserError
import com.ethowitz.scruby.lexer._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

object RubyParser extends Parsers {
  override type Elem = Token

  def apply(tokens: Seq[Token]): Either[ParserError, List[SyntaxTree]] = {
    val reader = new TokenReader(tokens)

    phrase(repsep(definition | expression, SeparatorToken))(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  val keywords = List("def", "end", "true", "false", "class", "if", "elsif", "unless", "else",
    "nil")

  def conditional: Parser[SyntaxTree] = iff.|[SyntaxTree](unless)

  def definition: Parser[SyntaxTree] = klassDef.|[SyntaxTree](methodDef)

  def elsif: Parser[List[SyntaxTree] => IfNode] = {
    (elsifToken ~> expression ~ SeparatorToken ~ sequence(expression)) ^^ {
      case predicate ~ _ ~ yesBranch =>
        (noBranch: List[SyntaxTree]) => IfNode(predicate, yesBranch, noBranch)
    }
  }

  def ivarAssignment: Parser[SyntaxTree] = {
    (IvarPrefixToken ~> (identifier.|[SyntaxTree](constant)) ~ AssignerToken ~ expression) ^^ {
      case IdentifierNode(name) ~ _ ~ value => IvarAssignmentNode(name, value)
    }
  }

  def localVarAssignment: Parser[SyntaxTree] =
    (nonKeywordIdentifier ~ AssignerToken ~ expression) ^^ {
      case IdentifierNode(name) ~ _ ~ value => LocalVarAssignmentNode(name, value)
    }

  def expression: Parser[SyntaxTree] = ivarAssignment | localVarAssignment | conditional |
    invocation | ivarIdentifier | nonKeywordIdentifier | literal

  def sequence[A](parser: Parser[A]): Parser[List[A]] = {
    (repsep(parser, SeparatorToken) <~ SeparatorToken).? ^^ {
      case Some(as) => as
      case None => List.empty[A]
    }
  }

  def defToken: Parser[Token] = elem(IdentifierToken("def"))
  def endToken: Parser[Token] = elem(IdentifierToken("end"))
  def klassToken: Parser[Token] = elem(IdentifierToken("class"))
  def ifToken: Parser[Token] = elem(IdentifierToken("if"))
  def elsifToken: Parser[Token] = elem(IdentifierToken("elsif"))
  def elseToken: Parser[Token] = elem(IdentifierToken("else"))
  def unlessToken: Parser[Token] = elem(IdentifierToken("unless"))

  def nonKeywordIdentifier: Parser[IdentifierNode] = accept("identifier", {
    case IdentifierToken(name) if !(keywords contains name) => IdentifierNode(Symbol(name))
  })

  def constant: Parser[ConstantNode] =
    accept("constant", { case ConstantToken(name) => ConstantNode(Symbol(name)) })

  def identifier: Parser[IdentifierNode] =
    accept("identifier", { case IdentifierToken(name) => IdentifierNode(Symbol(name)) })

  def ivarIdentifier: Parser[IvarIdentifierNode] = (IvarPrefixToken ~> identifier) ^^ {
    case IdentifierNode(name) => IvarIdentifierNode(name)
  }

  def iff: Parser[IfNode] = (ifToken ~> expression ~ SeparatorToken ~ sequence(expression) ~
    rep(elsif) ~ (elseToken ~ SeparatorToken ~ sequence(expression)).? ~ endToken) ^^ {

    case predicate ~ _ ~ yesBranch ~ elsifs ~ Some(_ ~ _~ noBranch) ~ _ =>
      IfNode(
        predicate,
        yesBranch,
        elsifs.foldRight(noBranch) { (acc: List[SyntaxTree] => IfNode, ifExp: List[SyntaxTree]) =>
          List(acc(ifExp))
        })
    case predicate ~ _ ~ yesBranch ~ elsifs ~ None ~ _ =>
      IfNode(predicate, yesBranch, elsifs.foldRight(List.empty[IfNode]) { (acc, ifExp) =>
        List(acc(ifExp))
      })
  }

  def invocation: Parser[InvocationNode] =
    (leftmostReceiver ~ PeriodToken ~ message(identifier)) ^^ {
      case recv ~ _ ~ inv => inv(Some(recv))
    }

  def klassDef: Parser[KlassDefNode] = {
    (klassToken ~> constant ~ SeparatorToken ~ sequence(definition) ~ endToken) ^^ {
      case ConstantNode(name) ~ _ ~ expressions ~  _ => KlassDefNode(name, expressions)
    }
  }

  def literal: Parser[SyntaxTree] = {
    accept("literal", {
      case StringToken(s) => StringNode(s)
      case SymbolToken(s) => SymbolNode(Symbol(s))
      case IntegerToken(n) => IntegerNode(n)
      case FloatToken(n) => FloatNode(n)
      case IdentifierToken("true") => TrueNode
      case IdentifierToken("false") => FalseNode
      case IdentifierToken("nil") => NilNode
    })
  }

  def message(id: Parser[IdentifierNode]): Parser[Option[SyntaxTree] => InvocationNode] =
    messageParens(id) | messageNoParens(id)

  def messageNoParens(id: Parser[IdentifierNode]): Parser[Option[SyntaxTree] => InvocationNode] = {
    (id ~ repsep(expression, CommaToken)) ^^ {
      case IdentifierNode(name) ~ args => ts => InvocationNode(ts, name, args)
    }
  }

  def messageParens(id: Parser[IdentifierNode]): Parser[Option[SyntaxTree] => InvocationNode] = {
    (id ~ OpeningParenthesisToken ~ repsep(expression, CommaToken) ~ ClosingParenthesisToken) ^^ {
      case IdentifierNode(name) ~ _ ~ args ~ _ =>
        (self: Option[SyntaxTree]) => InvocationNode(self, name, args)
    }
  }

  def methodDef: Parser[MethodDefNode] = {
    (defToken ~> identifier ~ (OpeningParenthesisToken ~ repsep(nonKeywordIdentifier, CommaToken) ~
      ClosingParenthesisToken).? ~ SeparatorToken ~ sequence(expression) ~ endToken) ^^ {

      case IdentifierNode(name) ~ Some(_ ~ params ~ _) ~ _ ~ expressions ~ _ =>
        MethodDefNode(name, params.map(_.name), expressions)
      case IdentifierNode(name) ~ None ~ _ ~ expressions ~ _ =>
        MethodDefNode(name, List.empty[Symbol], expressions)
    }
  }

  def leftmostReceiver: Parser[SyntaxTree] = {
    ((literal | conditional | nonKeywordIdentifier | ivarIdentifier | constant) ~
      (PeriodToken ~ message(identifier) ~ receiver).?) ^^ {

      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
      case _ => throw new Exception("unimplemented")
    }
  }

  def receiver: Parser[SyntaxTree] = {
    ((literal | conditional | identifier) ~ (PeriodToken ~ message(identifier) ~ receiver).?) ^^ {
      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
      case _ => throw new Exception("unimplemented")
    }
  }

  def unless: Parser[UnlessNode] = {
    (unlessToken ~ expression ~ SeparatorToken ~ sequence(expression) ~ endToken) ^^ {
      case _ ~ predicate ~ _ ~ expressions ~ _ => UnlessNode(predicate, expressions)
    }
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.headOption match {
    case Some(token) => token
    case None => throw new Exception("attempted to retrieve token from empty list")
  }

  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.drop(1))
}
