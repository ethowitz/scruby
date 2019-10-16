package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.core.ScrubyObject

class KlassMap private(m: Map[Symbol, ScrubyObject]) {
  private val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, ScrubyObject)): KlassMap = new KlassMap(internalMap + t)
  // scalastyle:on method.name

  // def get: symbol => option[scrubymethod] = internalmap.get
  def get(key: Symbol): Option[ScrubyObject] = internalMap get key
}

object KlassMap {
  def apply(ts: (Symbol, ScrubyObject)*): KlassMap = new KlassMap(ts.toMap)
  def empty: KlassMap = new KlassMap(Map.empty[Symbol, ScrubyObject])
}
