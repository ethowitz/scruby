package org.sruby.exceptions

final case class LexerError(msg: String) extends CompilationError
