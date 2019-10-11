package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap

object ScrubyFalseClass extends ScrubyObject('FalseClass, None, MethodMap.empty) {
  override def toString: String = "false"
}
