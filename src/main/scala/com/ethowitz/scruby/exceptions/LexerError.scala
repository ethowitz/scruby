package com.ethowitz.scruby.exceptions

final case class LexerError(msg: String) extends CompilationError
