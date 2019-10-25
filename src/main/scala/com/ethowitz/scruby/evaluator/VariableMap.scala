package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.core.ScrubyObject

class VariableMap private(m: Map[Symbol, ScrubyObject]) {
  private val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, ScrubyObject)): VariableMap = new VariableMap(internalMap + t)
  // scalastyle:on method.name

  // def get: symbol => option[scrubymethod] = internalmap.get
  def get(key: Symbol): Option[ScrubyObject] = internalMap get key
  def keys = internalMap.keys
}

object VariableMap {
  def apply(ts: (Symbol, ScrubyObject)*): VariableMap = new VariableMap(ts.toMap)
  def empty: VariableMap = new VariableMap(Map.empty[Symbol, ScrubyObject])
}
