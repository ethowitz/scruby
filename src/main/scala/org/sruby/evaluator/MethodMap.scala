package org.sruby.evaluator

class MethodMap private(m: Map[Symbol, RubyMethod]) {
  private val internalMap = m

  // scalastyle:off method.name
  def +(t: (Symbol, RubyMethod)): MethodMap = new MethodMap(internalMap + t)

  def ++(that: MethodMap): MethodMap = new MethodMap(internalMap ++ that.internalMap)
  // scalastyle:on method.name

  def get(key: Symbol): Option[RubyMethod] = internalMap get key
}

object MethodMap {
  def apply(ts: (Symbol, RubyMethod)*): MethodMap = new MethodMap(ts.toMap)
  def empty: MethodMap = new MethodMap(Map.empty[Symbol, RubyMethod])
}
