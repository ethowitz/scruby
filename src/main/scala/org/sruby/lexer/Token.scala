package org.sruby.lexer

sealed trait Token

// scalastyle:off number.of.types
final case class StringToken(s: String) extends Token
final case class SymbolToken(s: String) extends Token
final case class IntegerToken(n: Integer) extends Token
final case class FloatToken(n: Float) extends Token
case object TrueToken extends Token
case object FalseToken extends Token
case object NotToken extends Token
case object NilToken extends Token
final case class IdentifierToken(name: String) extends Token
final case class ConstantToken(name: String) extends Token
case object KlassToken extends Token
case object ModuleToken extends Token
case object DefToken extends Token
case object IfToken extends Token
case object UnlessToken extends Token
case object ElsifToken extends Token
case object ElseToken extends Token
case object EndToken extends Token
case object SelfToken extends Token
case object AmpersandToken extends Token
case object CommentToken extends Token
case object ArrowToken extends Token
case object BackslashToken extends Token
case object SeparatorToken extends Token
case object WhitespaceToken extends Token
//case object PLUS extends Token
//case object MINUS extends Token
//case object MULTIPLIED_BY extends Token
//case object DIVIDED_BY extends Token
//case object AND extends Token
//case object OR extends Token
//case object EQUALS extends Token
//case object LESS_THAN extends Token
//case object GREATER_THAN extends Token
//case object BITWISE_AND extends Token
//case object BITWISE_OR extends Token
//case object MODULUS extends Token
case object AssignerToken extends Token
case object IvarPrefixToken extends Token
case object ScopeResolverToken extends Token
case object CommaToken extends Token
case object PeriodToken extends Token
case object OpeningParenthesisToken extends Token
case object ClosingParenthesisToken extends Token
case object OpeningCurlyBracketToken extends Token
case object ClosingCurlyBracketToken extends Token
case object OpeningSquareBracketToken extends Token
case object ClosingSquareBracketToken extends Token
//case object QuestionMark extends Token
case object ColonToken extends Token
