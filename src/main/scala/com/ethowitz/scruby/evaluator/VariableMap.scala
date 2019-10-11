package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.core.ScrubyObject

class VariableMap private(m: Map[Symbol, ScrubyObject]) {
  private val internalMap = m

  def +(t: (Symbol, ScrubyObject)) = new VariableMap(internalMap + t)
  def get(key: Symbol) = internalMap.get(key)
  def iterator = internalMap.iterator
}

object VariableMap {
  def apply(ts: (Symbol, ScrubyObject)*): VariableMap = new VariableMap(ts.toMap)
  def empty: VariableMap = new VariableMap(Map.empty[Symbol, ScrubyObject])
}
