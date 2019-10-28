package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.RubyMethod
import com.ethowitz.scruby.evaluator.VariableMap
import com.ethowitz.scruby.parser.RubyObjectContainerNode

class RubyObject(
  val klass: Symbol,
  val name: Option[Symbol],
  val ms: MethodMap,
  val ivars: VariableMap
) {
  def methods: MethodMap = predefMethods ++ ms

  def withMethod(method: (Symbol, RubyMethod)): RubyObject =
    RubyObject(klass, name, ms + method, ivars)
  def withIvar(ivar: (Symbol, RubyObject)): RubyObject =
    RubyObject(klass, name, ms, ivars + ivar)

  protected def predefMethods: MethodMap = {
    val _klass: (Symbol, RubyMethod) = 'class ->
      RubyMethod(RubyObjectContainerNode(RubyString(klass.name)))
    val _initialize: (Symbol, RubyMethod) = 'new ->
      RubyMethod(RubyObjectContainerNode(RubyObject(klass, name, ms, ivars)))
    val nil: (Symbol, RubyMethod) = Symbol("nil?") ->
      RubyMethod(RubyObjectContainerNode(RubyFalseClass))

    MethodMap(nil, _klass, _initialize)
  }
}

object RubyObject {
  def apply(klass: Symbol, name: Option[Symbol], ms: MethodMap, ivars: VariableMap): RubyObject =
    new RubyObject(klass, name, ms, ivars)

  def apply(klass: Symbol): RubyObject = apply(klass, None, MethodMap(), VariableMap())
  def apply(klass: Symbol, name: Symbol, ms: MethodMap, ivars: VariableMap): RubyObject =
    apply(klass, Some(name), ms, ivars)
}
