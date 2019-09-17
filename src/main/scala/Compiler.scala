package scruby

object Compiler {
  def apply(code: String): Either[CompilationError, List[SyntaxTree]] = {
    for {
      tokens <- Lexer(code).right
      trees <- ScrubyParser(tokens).right
    } yield trees
  }
}
