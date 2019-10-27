package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.VariableMap

object ScrubyFalseClass
  extends ScrubyObject('FalseClass, None, MethodMap.empty, VariableMap.empty) {

  override def toString: String = "false"
}
