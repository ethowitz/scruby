package org.sruby.evaluator

import org.sruby.core.RubyObject

object RubyMain extends RubyObject('Object, MethodMap.empty, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "main"
}
