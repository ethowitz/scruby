package com.ethowitz.scruby.exceptions

final case class ParserError(msg: String) extends CompilationError
