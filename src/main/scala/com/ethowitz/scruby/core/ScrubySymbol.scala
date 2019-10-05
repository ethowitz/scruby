package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.ScrubyMethod

class ScrubySymbol(val s: Symbol, ms: Map[Symbol, ScrubyMethod])
    extends ScrubyObject('Class, Some('Symbol), ms) {
  override def toString: String = s.toString.replace("'", "")
}

object ScrubySymbol {
  def apply(s: Symbol): ScrubySymbol = new ScrubySymbol(s, Map())
}
