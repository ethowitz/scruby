package org.sruby.evaluator

import org.sruby.core.RubyObject

class VariableMap private(m: Map[Symbol, RubyObject]) {
  protected val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, RubyObject)): VariableMap = new VariableMap(internalMap + t)

  def ==(that: VariableMap): Boolean = internalMap == that.internalMap
  // scalastyle:on method.name

  def get(key: Symbol): Option[RubyObject] = internalMap get key
}

object VariableMap {
  def apply(ts: (Symbol, RubyObject)*): VariableMap = new VariableMap(ts.toMap)
  def empty: VariableMap = new VariableMap(Map.empty[Symbol, RubyObject])
}
