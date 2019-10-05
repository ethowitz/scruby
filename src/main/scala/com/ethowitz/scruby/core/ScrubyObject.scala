package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.ScrubyMethod
import com.ethowitz.scruby.parser.ScrubyObjectContainer

class ScrubyObject(val klass: Symbol, val name: Option[Symbol], val ms: Map[Symbol, ScrubyMethod]) {
  def methods: Map[Symbol, ScrubyMethod] = predefMethods ++ ms

  protected def predefMethods: Map[Symbol, ScrubyMethod] = {
    val _klass: (Symbol, ScrubyMethod) = 'class ->
      ScrubyMethod(ScrubyObjectContainer(ScrubyString(klass.name)))
    val _initialize: (Symbol, ScrubyMethod) = 'new ->
      ScrubyMethod(ScrubyObjectContainer(ScrubyObject(klass, name, ms)))
    val nil: (Symbol, ScrubyMethod) = Symbol("nil?") ->
      ScrubyMethod(ScrubyObjectContainer(ScrubyFalseClass))

    Map(nil, _klass, _initialize)
  }
}

object ScrubyObject {
  def apply(klass: Symbol, name: Option[Symbol], ms: Map[Symbol, ScrubyMethod]): ScrubyObject =
    new ScrubyObject(klass, name, ms)

  def apply(klass: Symbol): ScrubyObject = apply(klass, None, Map[Symbol, ScrubyMethod]())
  def apply(klass: Symbol, ms: Map[Symbol, ScrubyMethod]): ScrubyObject = apply(klass, None, ms)
  def apply(klass: Symbol, name: Symbol, ms: Map[Symbol, ScrubyMethod]): ScrubyObject =
    apply(klass, Some(name), ms)
}
