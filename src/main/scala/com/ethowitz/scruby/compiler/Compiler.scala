package com.ethowitz.scruby.compiler

import com.ethowitz.scruby.exceptions.CompilationError
import com.ethowitz.scruby.lexer.Lexer
import com.ethowitz.scruby.parser.{ RubyParser, SyntaxTree }

object Compiler {
  // Surpressing inferred Product type warnings for now due to what I think may be a bug:
  // https://github.com/scala/bug/issues/5589#issuecomment-297488689
  @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
  def apply(code: String): Either[CompilationError, List[SyntaxTree]] = {
    for {
      tokens <- Lexer(code).right
      trees <- RubyParser(tokens).right
    } yield trees
  }
}
