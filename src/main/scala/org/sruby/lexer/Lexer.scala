package org.sruby.lexer

import org.sruby.exceptions.LexerError
import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  override def skipWhitespace: Boolean = false

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code.trim) match {
      case NoSuccess(msg, _) => Left(LexerError(msg))
      case Success(result, _) => Right(result.filter(_ != WhitespaceToken))
    }
  }

  def ampersand: Parser[Token] = "&" ^^ (_ => AmpersandToken)
  def assigner: Parser[Token] = "=" ^^ (_ => AssignerToken)
  def ivarPrefix: Parser[Token] = "@" ^^ (_ => IvarPrefixToken)
  def scopeResolver: Parser[Token] = "::" ^^ (_ => ScopeResolverToken)
  def comma: Parser[Token] = "," ^^ (_ => CommaToken)
  def period: Parser[Token] = "." ^^ (_ => PeriodToken)
  def openingParenthesis: Parser[Token] = "(" ^^ (_ => OpeningParenthesisToken)
  def closingParenthesis: Parser[Token] = ")" ^^ (_ => ClosingParenthesisToken)
  def openingCurlyBracket: Parser[Token] = "{" ^^ (_ => OpeningCurlyBracketToken)
  def closingCurlyBracket: Parser[Token] = "}" ^^ (_ => ClosingCurlyBracketToken)
  def openingSquareBracket: Parser[Token] = "[" ^^ (_ => OpeningSquareBracketToken)
  def closingSquareBracket: Parser[Token] = "]" ^^ (_ => ClosingSquareBracketToken)
  def not: Parser[Token] = "!" ^^ (_ => NotToken)
  def colon: Parser[Token] = ":" ^^ (_ => ColonToken)
  def arrow: Parser[Token] = "=>" ^^ (_ => ArrowToken)
  def backslash: Parser[Token] = "\\" ^^ (_ => BackslashToken)
  def separator: Parser[Token] = "[\n|;|\r\f]+".r ^^ (_ => SeparatorToken)
  def whitespace: Parser[Token] = "[ ]+".r ^^ (_ => WhitespaceToken)

  def constant: Parser[ConstantToken] = {
    "[A-Z_=<>%&\\*\\|][a-zA-Z0-9_]*[?!]?".r ^^ { s => ConstantToken(s) }
  }

  def identifier: Parser[IdentifierToken] = {
    "[a-z_=<>%&\\*\\|][a-zA-Z0-9_]*[?!=]?".r ^^ { s => IdentifierToken(s) }
  }

  def string: Parser[StringToken] = {
    "'[^']*'".r ^^ { s => StringToken(s.substring(1, s.length - 1)) }
  }

  def symbol: Parser[SymbolToken] = {
    ":[a-zA-Z_=<>%&\\*\\|][a-zA-Z0-9_]*[?!]?".r ^^ { s => SymbolToken(s) }
  }

  def integer: Parser[IntegerToken] = {
    "0|[1-9]+[0-9]*".r ^^ { n => IntegerToken(n.toInt) }
  }

  def float: Parser[FloatToken] = {
    "(0|[1-9]+[0-9]*)+\\.[0-9]+".r ^^ { n => FloatToken(n.toFloat) }
  }

  // this heinous syntax appeases scalastyle, which failed to lex `string |[Token] symbol`:
  // `Expected token SEMI but got Token(LBRACKET,[,2416,[)'
  private def parsingGroup: Parser[Token] = string.|[Token](symbol) | integer | float | assigner |
    ampersand | constant | ivarPrefix | identifier | scopeResolver | comma | period |
    openingParenthesis | closingParenthesis | openingCurlyBracket | closingCurlyBracket |
    openingSquareBracket | closingSquareBracket | not | colon | separator | whitespace

  private def tokens: Parser[List[Token]] = phrase(rep1(parsingGroup))
}
