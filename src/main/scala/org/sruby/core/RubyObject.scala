package org.sruby.core

import org.sruby.evaluator.MethodMap
import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.VariableMap
import org.sruby.parser.RubyObjectContainerNode

class RubyObject(
  val klass: Symbol,
  private val instanceMethods: MethodMap,
  private val klassMethods: MethodMap,
  val ivars: VariableMap
) {
  // Public members
  def definingContext: Symbol = klass

  def methods: MethodMap = predefInstanceMethods ++ instanceMethods

  // this should not be a runtime error
  def newInstance: RubyObject = throw new Exception("attempted to instantiate object")

  def withInstanceMethod(t: (Symbol, RubyMethod)): RubyObject =
    RubyObject(klass, instanceMethods + t, klassMethods, ivars)

  def withIvar(t: (Symbol, RubyObject)): RubyObject =
    RubyObject(klass, instanceMethods, klassMethods, ivars + t)

  def withKlassMethod(t: (Symbol, RubyMethod)): RubyObject =
    RubyObject(klass, instanceMethods, klassMethods + t, ivars)

  // Protected members
  protected def predefInstanceMethods: MethodMap = {
    val _klass: (Symbol, RubyMethod) = 'class ->
      RubyMethod(RubyObjectContainerNode(RubyString(klass.name.toString)))
    val nil: (Symbol, RubyMethod) = Symbol("nil?") ->
      RubyMethod(RubyObjectContainerNode(RubyFalseClass))

    MethodMap(nil, _klass)
  }
}

object RubyObject {
  def apply(
    klass: Symbol, instanceMethods: MethodMap, klassMethods: MethodMap, ivars: VariableMap
  ): RubyObject = new RubyObject(klass, instanceMethods, klassMethods, ivars)

  def apply(klass: Symbol): RubyObject =
    apply(klass, MethodMap.empty, MethodMap.empty, VariableMap.empty)
}
