package com.ethowitz.scruby.evaluator

class MethodMap private(m: Map[Symbol, ScrubyMethod]) {
  private val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, ScrubyMethod)): MethodMap = t match {
    case ('initialize, method) => new MethodMap(internalMap + ('new -> ScrubyConstructor(method)))
    case tuple => new MethodMap(internalMap + tuple)
  }

  def ++(other: MethodMap): MethodMap = new MethodMap(internalMap ++ other.internalMap)
  // scalastyle:on method.name

  def get(key: Symbol): Option[ScrubyMethod] = internalMap get key
}

object MethodMap {
  def apply(ts: (Symbol, ScrubyMethod)*): MethodMap = new MethodMap(ts.toMap)
  def empty: MethodMap = new MethodMap(Map.empty[Symbol, ScrubyMethod])
}
