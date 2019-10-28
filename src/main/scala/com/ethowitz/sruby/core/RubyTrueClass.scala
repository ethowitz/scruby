package com.ethowitz.sruby.core

import com.ethowitz.sruby.evaluator.MethodMap
import com.ethowitz.sruby.evaluator.VariableMap

object RubyTrueClass extends RubyObject('TrueClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "true"
}
