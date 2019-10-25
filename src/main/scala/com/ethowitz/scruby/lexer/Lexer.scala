package com.ethowitz.scruby.lexer

import com.ethowitz.scruby.exceptions.LexerError
import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  override def skipWhitespace: Boolean = false

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code.trim) match {
      case NoSuccess(msg, _) => Left(LexerError(msg))
      case Success(result, _) => Right(result.filter(_ != Whitespace))
    }
  }

  def ampersand: Parser[Token] = "&" ^^ (_ => Ampersand)
  def assigner: Parser[Token] = "=" ^^ (_ => Assigner)
  def ivarPrefix: Parser[Token] = "@" ^^ (_ => IvarPrefix)
  def scopeResolver: Parser[Token] = "::" ^^ (_ => ScopeResolver)
  def comma: Parser[Token] = "," ^^ (_ => Comma)
  def period: Parser[Token] = "." ^^ (_ => Period)
  def openingParenthesis: Parser[Token] = "(" ^^ (_ => OpeningParenthesis)
  def closingParenthesis: Parser[Token] = ")" ^^ (_ => ClosingParenthesis)
  def openingCurlyBracket: Parser[Token] = "{" ^^ (_ => OpeningCurlyBracket)
  def closingCurlyBracket: Parser[Token] = "}" ^^ (_ => ClosingCurlyBracket)
  def openingSquareBracket: Parser[Token] = "[" ^^ (_ => OpeningSquareBracket)
  def closingSquareBracket: Parser[Token] = "]" ^^ (_ => ClosingSquareBracket)
  def not: Parser[Token] = "!" ^^ (_ => Not)
  def colon: Parser[Token] = ":" ^^ (_ => Colon)
  def arrow: Parser[Token] = "=>" ^^ (_ => Arrow)
  def backslash: Parser[Token] = "\\" ^^ (_ => Backslash)
  def separator: Parser[Token] = "[\n|;|\r\f]+".r ^^ (_ => Separator)
  def whitespace: Parser[Token] = "[ ]+".r ^^ (_ => Whitespace)

  def constant: Parser[ConstantToken] = {
    "[A-Z_=<>%&\\*\\|][a-zA-Z0-9_]*[?!]?".r ^^ { s => ConstantToken(s) }
  }

  def identifier: Parser[IdentifierToken] = {
    "[a-z_=<>%&\\*\\|][a-zA-Z0-9_]*[?!]?".r ^^ { s => IdentifierToken(s) }
  }

  def string: Parser[StringLiteral] = {
    "'[^']*'".r ^^ { s => StringLiteral(s.substring(1, s.length - 1)) }
  }

  def symbol: Parser[SymbolLiteral] = {
    ":[a-zA-Z_=<>%&\\*\\|][a-zA-Z0-9_]*[?!]?".r ^^ { s => SymbolLiteral(s) }
  }

  def integer: Parser[IntegerLiteral] = {
    "0|[1-9]+[0-9]*".r ^^ { n => IntegerLiteral(n.toInt) }
  }

  def float: Parser[FloatLiteral] = {
    "(0|[1-9]+[0-9]*)+\\.[0-9]+".r ^^ { n => FloatLiteral(n.toFloat) }
  }

  private def parsingGroup = string | symbol | integer | float | assigner | ampersand | constant |
    ivarPrefix | identifier | scopeResolver | comma | period | openingParenthesis |
    closingParenthesis | openingCurlyBracket | closingCurlyBracket | openingSquareBracket |
    closingSquareBracket | not | colon | separator | whitespace


  private def tokens: Parser[List[Token]] = phrase(rep1(parsingGroup))
}
