package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.core.RubyObject

class KlassMap private(m: Map[Symbol, RubyObject]) {
  private val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, RubyObject)): KlassMap = new KlassMap(internalMap + t)
  // scalastyle:on method.name

  // def get: symbol => option[srubymethod] = internalmap.get
  def get(key: Symbol): Option[RubyObject] = internalMap get key
}

object KlassMap {
  def apply(ts: (Symbol, RubyObject)*): KlassMap = new KlassMap(ts.toMap)
  def empty: KlassMap = new KlassMap(Map.empty[Symbol, RubyObject])
}
