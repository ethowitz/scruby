package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.VariableMap

class ScrubyString(val s: String, ms: MethodMap) extends ScrubyObject('Class, Some('String), ms, VariableMap.empty) {
  override def toString: String = s
}

object ScrubyString {
  def apply(s: String, ms: MethodMap): ScrubyString = new ScrubyString(s, ms)
  def apply(s: String): ScrubyString = new ScrubyString(s, MethodMap.empty)
}
