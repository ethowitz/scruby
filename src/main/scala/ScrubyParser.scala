package scruby

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object ScrubyParser extends Parsers {
  override type Elem = Token

  def apply(tokens: Seq[Token]): Either[ParserError, SyntaxTree] = {
    val reader = new TokenReader(tokens)
    phrase(definition | expression)(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  private def conditional: Parser[Expression] = iff | unless

  private def definition: Parser[SyntaxTree] = klassDef | methodDef

  private def elsif: Parser[List[Expression] => If] = {
    (Elsif ~> expression ~ (expression).*) ^^ {
      case predicate ~ yesBranch => If.curried(predicate)(yesBranch)
    }
  }

  private def expression: Parser[Expression] = conditional | invocation | literal

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case id @ IdentifierToken(name) => Identifier(name) })
  }

  private def iff: Parser[If] = {
    (IfToken ~> expression ~ rep(expression) ~ rep(elsif) ~ (Else ~> rep(expression)).? <~ End) ^^ {
      case predicate ~ yesBranch ~ elsifs ~ Some(noBranch) => {
        If(predicate, yesBranch, elsifs.foldRight(noBranch) { (acc, ifExp) => List(acc(ifExp)) })
      }
      case predicate ~ yesBranch ~ elsifs ~ None => {
        If(predicate, yesBranch, elsifs.foldRight(List[If]()) { (acc, ifExp) => List(acc(ifExp)) })
      }
    }
  }

  private def invocation: Parser[Invocation] = {
    ((receiver ~ Period).? ~ message) ^^ {
      case Some(recv ~ _) ~ invWithoutReceiver => invWithoutReceiver(recv)
      case None ~ invWithoutReceiver => invWithoutReceiver(null) 
    }
  }

  private def klassDef: Parser[KlassDef] = {
    (Klass ~> identifier ~ rep(methodDef) <~ End) ^^ {
      case Identifier(name) ~ methodDefs => KlassDef(name, methodDefs)
    }
  }

  private def literal: Parser[Literal] = {
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

  private def message: Parser[Expression => Invocation] = messageParens | messageNoParens

  private def messageNoParens: Parser[Expression => Invocation] = {
    (identifier ~ repsep(expression, Comma)) ^^ {
      case Identifier(name) ~ args => Invocation.curried(_: Expression)(name)(args)
    }
  }

  private def messageParens: Parser[Expression => Invocation] = {
    (identifier ~ OpeningParenthesis ~ repsep(expression, Comma) ~ ClosingParenthesis) ^^ {
      case Identifier(name) ~ _ ~ args ~ _ => Invocation.curried(_: Expression)(name)(args)
    }
  }

  private def methodDef: Parser[MethodDef] = {
    (Def ~> identifier ~ (OpeningParenthesis ~ repsep(identifier, Comma) ~ ClosingParenthesis).? ~ rep(expression) <~ End) ^^ {
      case Identifier(name) ~ Some(_ ~ params ~ _) ~ expressions => MethodDef(name, params.map(_.name), expressions)
      case Identifier(name) ~ None ~ expressions => MethodDef(name, List[String](), expressions)
    }
  }

  private def receiver: Parser[Expression] = {
    ((literal | conditional | identifier) ~ (Period ~ message ~ receiver).?) ^^ {
      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
    }
  }

  private def unless: Parser[Unless] = {
    (UnlessToken ~> expression ~ rep(expression) <~ End) ^^ {
      case predicate ~ expressions => Unless(predicate, expressions)
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
