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

  private def conditional: Parser[SyntaxTree] = iff | unless

  private def definition: Parser[SyntaxTree] = klassDef | methodDef

  private def elsif: Parser[List[SyntaxTree] => If] = {
    (Elsif ~> expression ~ Separator ~ sequence(expression)) ^^ {
      case predicate ~ _ ~ yesBranch => If.curried(predicate)(yesBranch)
    }
  }

  private def expression: Parser[SyntaxTree] = conditional | invocation | literal

  private def sequence[A](parser: Parser[A]): Parser[List[A]] = {
    (repsep(parser, Separator) <~ Separator).? ^^ {
      case Some(exps) => exps
      case None => List[A]()
    }
  }

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case id @ IdentifierToken(name) => Identifier(name) })
  }

  private def iff: Parser[If] = {
    (IfToken ~> expression ~ Separator ~ sequence(expression) ~ rep(elsif) ~
      (Else ~ Separator ~ sequence(expression)).? ~ End) ^^ {
      case predicate ~ _ ~ yesBranch ~ elsifs ~ Some(_ ~ _~ noBranch) ~ _ => {
        If(predicate, yesBranch, elsifs.foldRight(noBranch) { (acc, ifExp) => List(acc(ifExp)) })
      }
      case predicate ~ _ ~ yesBranch ~ elsifs ~ None ~ _ => {
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
    (Klass ~> identifier ~ Separator ~ sequence(definition) ~ End) ^^ {
      case Identifier(name) ~ _ ~ expressions ~  _ => KlassDef(name, expressions)
    }
  }

  private def literal: Parser[SyntaxTree] = {
    accept("literal", {
      case StringLiteral(s) => String_(s)
      case SymbolLiteral(s) => Symbol_(Symbol(s))
      case IntegerLiteral(n) => Integer_(n)
      case FloatLiteral(n) => Float_(n)
      case TrueLiteral => True
      case FalseLiteral => False
      case NilLiteral => Nil_
    })
  }

  private def message: Parser[SyntaxTree => Invocation] = messageParens | messageNoParens

  private def messageNoParens: Parser[SyntaxTree => Invocation] = {
    (identifier ~ repsep(expression, Comma)) ^^ {
      case Identifier(name) ~ args => Invocation.curried(_: SyntaxTree)(name)(args)
    }
  }

  private def messageParens: Parser[SyntaxTree => Invocation] = {
    (identifier ~ OpeningParenthesis ~ repsep(expression, Comma) ~ ClosingParenthesis) ^^ {
      case Identifier(name) ~ _ ~ args ~ _ => Invocation.curried(_: SyntaxTree)(name)(args)
    }
  }

  private def methodDef: Parser[MethodDef] = {
    (Def ~> identifier ~ (OpeningParenthesis ~ repsep(identifier, Comma) ~ ClosingParenthesis).? ~
        Separator ~ sequence(expression) ~ End) ^^ {
      case Identifier(name) ~ Some(_ ~ params ~ _) ~ _ ~ expressions ~ _ =>
        MethodDef(name, params.map(_.name), expressions)
      case Identifier(name) ~ None ~ _ ~ expressions ~ _ =>
        MethodDef(name, List[String](), expressions)
    }
  }

  private def receiver: Parser[SyntaxTree] = {
    ((literal | conditional | identifier) ~ (Period ~ message ~ receiver).?) ^^ {
      //case recv ~ Some(_ ~ invWithoutReceiver ~ arg) => invWithoutReceiver(recv)
      case recv ~ None => recv
    }
  }

  private def unless: Parser[Unless] = {
    (UnlessToken ~ expression ~ Separator ~ sequence(expression) ~ End) ^^ {
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
