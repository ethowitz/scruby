package org.sruby.core

import org.sruby.evaluator.MethodMap
import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.VariableMap
import org.sruby.parser.RubyObjectContainerNode

object RubyNilClass extends RubyObject(
  'NilClass, MethodMap.empty, MethodMap.empty, VariableMap.empty
) {
  override def toString: String = "nil"

  override def predefInstanceMethods: MethodMap =
    super.predefInstanceMethods +
      (Symbol("nil?") -> RubyMethod(RubyObjectContainerNode(RubyTrueClass)))
}
