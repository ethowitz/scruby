package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap

object ScrubyTrueClass extends ScrubyObject('TrueClass, None, MethodMap.empty) {
  override def toString: String = "true"
}
