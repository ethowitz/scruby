package scruby

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object ScrubyParser extends Parsers {
  override type Elem = Token
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

