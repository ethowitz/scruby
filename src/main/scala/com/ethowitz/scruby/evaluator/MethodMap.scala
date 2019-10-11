package com.ethowitz.scruby.evaluator

class MethodMap private(m: Map[Symbol, ScrubyMethod]) {
  private val internalMap = m

  def +(t: (Symbol, ScrubyMethod)) = new MethodMap(internalMap + t)
  def ++(other: MethodMap) = new MethodMap(internalMap ++ other.internalMap)

  def get(key: Symbol) = internalMap get key match {
    case Some(method) => method
    case None => throw new Exception(s"method ${key.toString} not found")
  }

  def iterator = internalMap.iterator
}

object MethodMap {
  def apply(ts: (Symbol, ScrubyMethod)*): MethodMap = new MethodMap(ts.toMap)
  def empty: MethodMap = new MethodMap(Map.empty[Symbol, ScrubyMethod])
}
