package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.VariableMap

class RubySymbol(val s: Symbol, ms: MethodMap)
  extends RubyObject('Class, Some('Symbol), ms, VariableMap.empty) {

  override def toString: String = s.toString.replace("'", "")
}

object RubySymbol {
  def apply(s: Symbol): RubySymbol = new RubySymbol(s, MethodMap.empty)
}
