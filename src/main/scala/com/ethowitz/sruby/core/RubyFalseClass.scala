package com.ethowitz.sruby.core

import com.ethowitz.sruby.evaluator.MethodMap
import com.ethowitz.sruby.evaluator.VariableMap

object RubyFalseClass extends RubyObject('FalseClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "false"
}
