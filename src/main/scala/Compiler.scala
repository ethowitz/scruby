package scruby

object Compiler {
  def apply(code: String): Either[CompilationError, SyntaxTree] = {
    for {
      tokens <- Lexer(code).right
      syntaxTree <- ScrubyParser(tokens).right
    } yield syntaxTree
  }
}
