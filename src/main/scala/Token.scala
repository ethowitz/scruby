package scruby

sealed trait Token

case class StringLiteral(s: String) extends Token
case class SymbolLiteral(s: String) extends Token
case class IntegerLiteral(n: Integer) extends Token
case class FloatLiteral(n: Float) extends Token
case object TrueLiteral extends Token
case object FalseLiteral extends Token
case object Not extends Token
case object NilLiteral extends Token
case class IdentifierToken(name: String) extends Token
case object Klass extends Token
case object Module extends Token
case object Def extends Token
case object IfToken extends Token
case object UnlessToken extends Token
case object Elsif extends Token
case object Else extends Token
case object End extends Token
case object Ampersand extends Token
case object Comment extends Token 
case object Arrow extends Token
case object Backslash extends Token
case object Separator extends Token
case object Whitespace extends Token
// TODO: syntactic sugar for the following method invocations:
//
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
//case object Assigner extends Token
case object IvarPrefix extends Token
case object MethodInvoker extends Token
case object ScopeResolver extends Token
case object Comma extends Token
case object Period extends Token
case object OpeningParenthesis extends Token
case object ClosingParenthesis extends Token
case object OpeningCurlyBracket extends Token
case object ClosingCurlyBracket extends Token
case object OpeningSquareBracket extends Token
case object ClosingSquareBracket extends Token
case object QuestionMark extends Token
case object Colon extends Token
