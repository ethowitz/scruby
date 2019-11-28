package org.sruby.core

import org.sruby.evaluator.MethodMap

class RubyString(val s: String, instanceMethods: MethodMap, klassMethods: MethodMap)
  extends RubyKlass('String, instanceMethods, klassMethods) {

  override def toString: String = s
}

object RubyString {
  def apply(s: String, instanceMethods: MethodMap, klassMethods: MethodMap): RubyString =
    new RubyString(s, instanceMethods, klassMethods)

  def apply(s: String): RubyString = new RubyString(s, MethodMap.empty, MethodMap.empty)
}
