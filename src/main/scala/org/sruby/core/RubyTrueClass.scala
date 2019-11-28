package org.sruby.core

import org.sruby.evaluator.MethodMap
import org.sruby.evaluator.VariableMap

object RubyTrueClass extends RubyObject(
  'TrueClass, MethodMap.empty, MethodMap.empty, VariableMap.empty
) {
  override def toString: String = "true"
}
