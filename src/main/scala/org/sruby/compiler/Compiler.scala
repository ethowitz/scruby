package org.sruby.compiler

import org.sruby.exceptions.CompilationError
import org.sruby.lexer.Lexer
import org.sruby.parser.{ RubyParser, AST }

object Compiler {
  // Surpressing inferred Product type warnings for now due to what I think may be a bug:
  // https://github.com/scala/bug/issues/5589#issuecomment-297488689
  @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
  def apply(code: String): Either[CompilationError, List[AST]] = {
    for {
      tokens <- Lexer(code).right
      trees <- RubyParser(tokens).right
    } yield trees
  }
}
