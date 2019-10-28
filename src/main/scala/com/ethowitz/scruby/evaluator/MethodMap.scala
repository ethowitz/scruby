package com.ethowitz.scruby.evaluator

class MethodMap private(m: Map[Symbol, RubyMethod]) {
  private val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, RubyMethod)): MethodMap = t match {
    case ('initialize, method) => new MethodMap(internalMap + ('new -> RubyConstructor(method)))
    case tuple => new MethodMap(internalMap + tuple)
  }

  def ++(other: MethodMap): MethodMap = new MethodMap(internalMap ++ other.internalMap)
  // scalastyle:on method.name

  def get(key: Symbol): Option[RubyMethod] = internalMap get key
}

object MethodMap {
  def apply(ts: (Symbol, RubyMethod)*): MethodMap = new MethodMap(ts.toMap)
  def empty: MethodMap = new MethodMap(Map.empty[Symbol, RubyMethod])
}
