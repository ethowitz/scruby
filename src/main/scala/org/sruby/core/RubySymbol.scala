package org.sruby.core

import org.sruby.evaluator.MethodMap

class RubySymbol(val s: Symbol, instanceMethods: MethodMap, klassMethods: MethodMap)
  extends RubyKlass('Symbol, instanceMethods, klassMethods) {

  override def toString: String = s.toString.replace("'", "")
}

object RubySymbol {
  def apply(s: Symbol): RubySymbol = new RubySymbol(s, MethodMap.empty, MethodMap.empty)
}
