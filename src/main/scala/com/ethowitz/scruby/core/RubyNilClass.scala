package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.RubyMethod
import com.ethowitz.scruby.evaluator.VariableMap
import com.ethowitz.scruby.parser.RubyObjectContainerNode

object RubyNilClass extends RubyObject('NilClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "nil"

  override def predefMethods: MethodMap =
    super.predefMethods + (Symbol("nil?") -> RubyMethod(RubyObjectContainerNode(RubyTrueClass)))
}
