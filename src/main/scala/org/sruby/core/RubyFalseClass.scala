package org.sruby.core

import org.sruby.evaluator.MethodMap
import org.sruby.evaluator.VariableMap

object RubyFalseClass extends RubyObject(
  'FalseClass, MethodMap.empty, MethodMap.empty, VariableMap.empty
) {
  override def toString: String = "false"
}
