package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.VariableMap

object RubyFalseClass
  extends RubyObject('FalseClass, None, MethodMap.empty, VariableMap.empty) {

  override def toString: String = "false"
}
