package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.MethodMap
import com.ethowitz.scruby.evaluator.ScrubyMethod

class ScrubySymbol(val s: Symbol, ms: MethodMap) extends ScrubyObject('Class, Some('Symbol), ms) {
  override def toString: String = s.toString.replace("'", "")
}

object ScrubySymbol {
  def apply(s: Symbol): ScrubySymbol = new ScrubySymbol(s, MethodMap.empty)
}
