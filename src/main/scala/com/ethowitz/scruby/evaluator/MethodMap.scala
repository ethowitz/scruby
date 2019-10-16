package com.ethowitz.scruby.evaluator

class MethodMap private(m: Map[Symbol, ScrubyMethod]) {
  private val internalMap = m

  def +(t: (Symbol, ScrubyMethod)): MethodMap = new MethodMap(internalMap + t)
  def ++(other: MethodMap): MethodMap = new MethodMap(internalMap ++ other.internalMap)

  // def get: Symbol => Option[ScrubyMethod] = internalMap.get
  def get(key: Symbol): Option[ScrubyMethod] = internalMap get key
}

object MethodMap {
  def apply(ts: (Symbol, ScrubyMethod)*): MethodMap = new MethodMap(ts.toMap)
  def empty: MethodMap = new MethodMap(Map.empty[Symbol, ScrubyMethod])
}
