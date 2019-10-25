package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.VariableMap

object ScrubyTrueClass extends ScrubyObject('TrueClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "true"
}
