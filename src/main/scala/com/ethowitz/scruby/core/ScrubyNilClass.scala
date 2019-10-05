package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.ScrubyMethod
import com.ethowitz.scruby.parser.ScrubyObjectContainer

object ScrubyNilClass extends ScrubyObject('NilClass, None, Map()) {
  override def toString: String = "nil"

  override def predefMethods: Map[Symbol, ScrubyMethod] =
    super.predefMethods + (Symbol("nil?") -> ScrubyMethod(ScrubyObjectContainer(ScrubyTrueClass)))
}
