package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.core.RubyObject

class VariableMap private(m: Map[Symbol, RubyObject]) {
  private val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, RubyObject)): VariableMap = new VariableMap(internalMap + t)
  // scalastyle:on method.name

  // def get: symbol => option[scrubymethod] = internalmap.get
  def get(key: Symbol): Option[RubyObject] = internalMap get key
}

object VariableMap {
  def apply(ts: (Symbol, RubyObject)*): VariableMap = new VariableMap(ts.toMap)
  def empty: VariableMap = new VariableMap(Map.empty[Symbol, RubyObject])
}
