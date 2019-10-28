package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.VariableMap

object RubyTrueClass extends RubyObject('TrueClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "true"
}
