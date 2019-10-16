package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.core.ScrubyObject

class VariableMap private(m: Map[Symbol, ScrubyObject]) {
  private val internalMap = m

  def +(t: (Symbol, ScrubyObject)) = new VariableMap(internalMap + t)

  // def get: symbol => option[scrubymethod] = internalmap.get
  def get(key: Symbol): Option[ScrubyObject] = internalMap get key
}

object VariableMap {
  def apply(ts: (Symbol, ScrubyObject)*): VariableMap = new VariableMap(ts.toMap)
  def empty: VariableMap = new VariableMap(Map.empty[Symbol, ScrubyObject])
}
