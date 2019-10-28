package com.ethowitz.sruby.exceptions

final case class ParserError(msg: String) extends CompilationError
