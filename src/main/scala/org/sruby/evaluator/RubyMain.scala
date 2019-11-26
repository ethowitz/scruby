package org.sruby.evaluator

import org.sruby.core.RubyObject

object RubyMain extends RubyObject('Object, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "main"
}
