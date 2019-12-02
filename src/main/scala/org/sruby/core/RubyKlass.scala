package org.sruby.core

import org.sruby.evaluator.{ MethodMap, RubyMethod, VariableMap }
import org.sruby.parser.RubyObjectContainerNode

class RubyKlass(
  val name: Symbol, private val instanceMethods: MethodMap, private val klassMethods: MethodMap
) extends RubyObject('Class, instanceMethods, klassMethods, VariableMap.empty) {
  // Public members
  override def definingContext: Symbol = name

  override def methods: MethodMap = predefKlassMethods ++ klassMethods

  override def newInstance: RubyObject =
    RubyObject(name, instanceMethods, klassMethods, VariableMap.empty)

  override def withInstanceMethod(t: (Symbol, RubyMethod)): RubyObject =
    RubyKlass(name, instanceMethods + t, klassMethods)

  override def withKlassMethod(t: (Symbol, RubyMethod)): RubyObject =
    RubyKlass(name, instanceMethods, klassMethods + t)

  // Protected members
  protected def predefKlassMethods: MethodMap = {
    val _initialize: (Symbol, RubyMethod) = 'new -> RubyMethod(RubyObjectContainerNode(newInstance))

    MethodMap(_initialize)
  }
}

object RubyKlass {
  def apply(name: Symbol): RubyKlass = new RubyKlass(name, MethodMap.empty, MethodMap.empty)

  def apply(name: Symbol, instanceMethods: MethodMap, klassMethods: MethodMap): RubyKlass =
    new RubyKlass(name, instanceMethods, klassMethods)
}
