package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.ScrubyMethod
import com.ethowitz.scruby.evaluator.VariableMap
import com.ethowitz.scruby.parser.ScrubyObjectContainer

class ScrubyObject(
  val klass: Symbol,
  val name: Option[Symbol],
  val ms: MethodMap,
  val ivars: VariableMap
) {
  def methods: MethodMap = predefMethods ++ ms

  def withMethod(method: (Symbol, ScrubyMethod)): ScrubyObject =
    ScrubyObject(klass, name, ms + method, ivars)
  def withIvar(ivar: (Symbol, ScrubyObject)): ScrubyObject =
    ScrubyObject(klass, name, ms, ivars + ivar)

  protected def predefMethods: MethodMap = {
    val _klass: (Symbol, ScrubyMethod) = 'class ->
      ScrubyMethod(ScrubyObjectContainer(ScrubyString(klass.name)))
    val _initialize: (Symbol, ScrubyMethod) = 'new ->
      ScrubyMethod(ScrubyObjectContainer(ScrubyObject(klass, name, ms, ivars)))
    val nil: (Symbol, ScrubyMethod) = Symbol("nil?") ->
      ScrubyMethod(ScrubyObjectContainer(ScrubyFalseClass))

    MethodMap(nil, _klass, _initialize)
  }
}

object ScrubyObject {
  def apply(klass: Symbol, name: Option[Symbol], ms: MethodMap, ivars: VariableMap): ScrubyObject =
    new ScrubyObject(klass, name, ms, ivars)

  def apply(klass: Symbol): ScrubyObject = apply(klass, None, MethodMap(), VariableMap())
  def apply(klass: Symbol, name: Symbol, ms: MethodMap, ivars: VariableMap): ScrubyObject =
    apply(klass, Some(name), ms, ivars)
}
