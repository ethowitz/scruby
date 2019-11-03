package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.core.RubyObject

object RubyMain extends RubyObject('Object, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "main"
}
