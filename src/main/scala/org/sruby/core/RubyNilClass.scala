package org.sruby.core

import org.sruby.evaluator.MethodMap
import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.VariableMap
import org.sruby.parser.RubyObjectContainerNode

object RubyNilClass extends RubyObject('NilClass, None, MethodMap.empty, VariableMap.empty) {
  override def toString: String = "nil"

  override def predefMethods: MethodMap =
    super.predefMethods + (Symbol("nil?") -> RubyMethod(RubyObjectContainerNode(RubyTrueClass)))
}
