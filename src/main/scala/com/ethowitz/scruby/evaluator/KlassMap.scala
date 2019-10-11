package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.core.ScrubyObject

class KlassMap private(m: Map[Symbol, ScrubyObject]) {
  private val internalMap = m

  def +(t: (Symbol, ScrubyObject)) = new KlassMap(internalMap + t)

  def get(key: Symbol) = internalMap get key match {
    case Some(method) => method
    case None => throw new Exception(s"class ${key.toString} not found")
  }

  def iterator = internalMap.iterator
}

object KlassMap {
  def apply(ts: (Symbol, ScrubyObject)*): KlassMap = new KlassMap(ts.toMap)
  def empty: KlassMap = new KlassMap(Map.empty[Symbol, ScrubyObject])
}
