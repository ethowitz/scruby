package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.ScrubyMethod
import com.ethowitz.scruby.evaluator.VariableMap
import com.ethowitz.scruby.parser.ScrubyObjectContainer

object ScrubyNilClass extends ScrubyObject('NilClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "nil"

  override def predefMethods: MethodMap =
    super.predefMethods + (Symbol("nil?") -> ScrubyMethod(ScrubyObjectContainer(ScrubyTrueClass)))
}
