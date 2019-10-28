package com.ethowitz.sruby.core

import com.ethowitz.sruby.evaluator.MethodMap
import com.ethowitz.sruby.evaluator.RubyMethod
import com.ethowitz.sruby.evaluator.VariableMap
import com.ethowitz.sruby.parser.RubyObjectContainerNode

object RubyNilClass extends RubyObject('NilClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "nil"

  override def predefMethods: MethodMap =
    super.predefMethods + (Symbol("nil?") -> RubyMethod(RubyObjectContainerNode(RubyTrueClass)))
}
