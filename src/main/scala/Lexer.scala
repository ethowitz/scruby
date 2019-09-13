package scruby

import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \r\f\n]+".r

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => Left(LexerError(msg))
      case Success(result, _) => Right(result)
    }
  }

  def klass = "class" ^^ (_ => Klass)
  def module = "module" ^^ (_ => Module)
  def def_ = "def" ^^ (_ => Def)
  def if_ = "if" ^^ (_ => If)
  def unless = "unless" ^^ (_ => Unless)
  def elsif = "elsif" ^^ (_ => Elsif)
  def else_ = "else" ^^ (_ => Else)
  def end = "end" ^^ (_ => End)
  def nil = "nil" ^^ (_ => Nil)
  def true_ = "true" ^^ (_ => TrueLiteral)
  def false_ = "false" ^^ (_ => FalseLiteral)
  def ampersand = "&" ^^ (_ => Ampersand)
  def ivarPrefix = "@" ^^ (_ => IvarPrefix)
  def methodInvoker = "." ^^ (_ => MethodInvoker)
  def scopeResolver = "::" ^^ (_ => ScopeResolver)
  def comma = "," ^^ (_ => Comma)
  def period = "." ^^ (_ => Period)
  def openingParenthesis = "(" ^^ (_ => OpeningParenthesis)
  def closingParenthesis = ")" ^^ (_ => ClosingParenthesis)
  def openingCurlyBracket = "{" ^^ (_ => OpeningCurlyBracket)
  def closingCurlyBracket = "}" ^^ (_ => ClosingCurlyBracket)
  def openingSquareBracket = "[" ^^ (_ => OpeningSquareBracket)
  def closingSquareBracket = "]" ^^ (_ => ClosingSquareBracket)
  def semicolon = ";" ^^ (_ => Semicolon)
  def questionMark = "?" ^^ (_ => QuestionMark)
  def not = "!" ^^ (_ => Not)
  def colon = ":" ^^ (_ => Colon)
  def arrow = "=>" ^^ (_ => Arrow)
  def backslash = "\\" ^^ (_ => Backslash)

  def identifier: Parser[Identifier] = {
    "[a-zA-Z_=<>%&\\*\\|][a-zA-Z0-9_]*[?\\?|?!]*".r ^^ { s => Identifier(s) }
  }

  def string: Parser[StringLiteral] = {
    "'[^']*'".r ^^ { s => StringLiteral(s.substring(1, s.length - 1)) }
  }

  def symbol: Parser[SymbolLiteral] = {
    ":[a-zA-Z_=<>%&\\*\\|][a-zA-Z0-9_]*[?\\?|?!]*".r ^^ { s => SymbolLiteral(s) }
  }

  def integer: Parser[IntegerLiteral] = {
    "0|[1-9]+[0-9]*".r ^^ { n => IntegerLiteral(n.toInt) }
  }

  def float: Parser[FloatLiteral] = {
    "(0|[1-9]+[0-9]*)+\\.[0-9]+".r ^^ { n => FloatLiteral(n.toFloat) }
  }

  private def parsing_group_1 = klass | module | def_ | if_ | unless | elsif |
    else_ | end | nil | true_ | false_ | string | symbol | identifier |
    integer | float

  private def parsing_group_2 = ampersand | ivarPrefix | methodInvoker |
    scopeResolver | comma | period | openingParenthesis | closingParenthesis |
    openingCurlyBracket | closingCurlyBracket | openingSquareBracket |
    closingSquareBracket | semicolon | questionMark | not | colon

  private def tokens: Parser[List[Token]] = phrase(rep1(parsing_group_1 | parsing_group_2))
}

trait CompilationError
case class LexerError(msg: String) extends CompilationError

