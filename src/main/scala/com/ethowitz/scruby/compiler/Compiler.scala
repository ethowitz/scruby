package com.ethowitz.scruby.compiler

import com.ethowitz.scruby.exceptions.CompilationError
import com.ethowitz.scruby.lexer.Lexer
import com.ethowitz.scruby.parser.ScrubyParser
import com.ethowitz.scruby.parser.SyntaxTree

object Compiler {
  def apply(code: String): Either[CompilationError, List[SyntaxTree]] = {
    for {
      tokens <- Lexer(code).right
      trees <- ScrubyParser(tokens).right
    } yield trees
  }
}
