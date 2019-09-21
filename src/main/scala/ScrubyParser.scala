package scruby

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

  private def conditional: Parser[SyntaxTree] = iff | unless

  private def definition: Parser[SyntaxTree] = klassDef | methodDef

  private def elsif: Parser[List[SyntaxTree] => If] = {
    (elsifToken ~> expression ~ Separator ~ sequence(expression)) ^^ {
      case predicate ~ _ ~ yesBranch => If.curried(predicate)(yesBranch)
    }
  }

  private def expression: Parser[SyntaxTree] = conditional | invocation | invocationWithoutReceiver | literal

  private def sequence[A](parser: Parser[A]): Parser[List[A]] = {
    (repsep(parser, Separator) <~ Separator).? ^^ {
      case Some(exps) => exps
      case None => List[A]()
    }
  }

  private def defToken: Parser[Token] = elem(IdentifierToken("def"))
  private def endToken: Parser[Token] = elem(IdentifierToken("end"))
  private def klassToken: Parser[Token] = elem(IdentifierToken("class"))
  private def ifToken: Parser[Token] = elem(IdentifierToken("if"))
  private def elsifToken: Parser[Token] = elem(IdentifierToken("elsif"))
  private def elseToken: Parser[Token] = elem(IdentifierToken("else"))
  private def unlessToken: Parser[Token] = elem(IdentifierToken("unless"))

  private def idsWithoutKeywords: Parser[Identifier] = {
    accept("identifier", { case IdentifierToken(name) if !(keywords contains name) => Identifier(Symbol(name)) })
  }

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case IdentifierToken(name) => Identifier(Symbol(name)) })
  }

  private def iff: Parser[If] = {
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

  private def invocationWithoutReceiver: Parser[Invocation] = {
    (message(idsWithoutKeywords)) ^^ {
      case invWithoutReceiver => invWithoutReceiver(None) 
    }
  }

  private def invocation: Parser[Invocation] = {
    (leftmostReceiver ~ Period ~ message(identifier)) ^^ {
      case recv ~ _ ~ inv => inv(Some(recv))
    }
  }

  private def klassDef: Parser[KlassDef] = {
    (klassToken ~> idsWithoutKeywords ~ Separator ~ sequence(definition) ~ endToken) ^^ {
      case Identifier(name) ~ _ ~ expressions ~  _ => KlassDef(name, expressions)
    }
  }

  private def literal: Parser[SyntaxTree] = {
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

  private def message(id: Parser[Identifier]): Parser[Option[SyntaxTree] => Invocation] = messageParens(id) | messageNoParens(id)

  private def messageNoParens(id: Parser[Identifier]): Parser[Option[SyntaxTree] => Invocation] = {
    (id ~ repsep(expression, Comma)) ^^ {
      case Identifier(name) ~ args => Invocation.curried(_: Option[SyntaxTree])(name)(args)
    }
  }

  private def messageParens(id: Parser[Identifier]): Parser[Option[SyntaxTree] => Invocation] = {
    (id ~ OpeningParenthesis ~ repsep(expression, Comma) ~ ClosingParenthesis) ^^ {
      case Identifier(name) ~ _ ~ args ~ _ => Invocation.curried(_: Option[SyntaxTree])(name)(args)
    }
  }

  private def methodDef: Parser[MethodDef] = {
    (defToken ~> identifier ~ (OpeningParenthesis ~ repsep(idsWithoutKeywords, Comma) ~ ClosingParenthesis).? ~
        Separator ~ sequence(expression) ~ endToken) ^^ {
      case Identifier(name) ~ Some(_ ~ params ~ _) ~ _ ~ expressions ~ _ =>
        MethodDef(name, params.map(_.name), expressions)
      case Identifier(name) ~ None ~ _ ~ expressions ~ _ =>
        MethodDef(name, List[Symbol](), expressions)
    }
  }

  private def leftmostReceiver: Parser[SyntaxTree] = {
    ((literal | conditional | idsWithoutKeywords) ~ (Period ~ message(identifier) ~ receiver).?) ^^ {
      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
    }
  }

  private def receiver: Parser[SyntaxTree] = {
    ((literal | conditional | identifier) ~ (Period ~ message(identifier) ~ receiver).?) ^^ {
      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
    }
  }

  private def unless: Parser[Unless] = {
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

case class ParserError(msg: String) extends CompilationError
