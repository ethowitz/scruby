package com.ethowitz.sruby.core

import com.ethowitz.sruby.evaluator.MethodMap
import com.ethowitz.sruby.evaluator.VariableMap

class RubyString(val s: String, ms: MethodMap)
  extends RubyObject('Class, Some('String), ms, VariableMap.empty) {

  override def toString: String = s
}

object RubyString {
  def apply(s: String, ms: MethodMap): RubyString = new RubyString(s, ms)
  def apply(s: String): RubyString = new RubyString(s, MethodMap.empty)
}
