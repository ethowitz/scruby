package com.ethowitz.scruby.core

import com.ethowitz.scruby.evaluator.ScrubyMethod

class ScrubyString(val s: String, ms: Map[Symbol, ScrubyMethod])
    extends ScrubyObject('Class, Some('String), ms) {
  override def toString: String = s
}

object ScrubyString {
  def apply(s: String, ms: Map[Symbol, ScrubyMethod]): ScrubyString = new ScrubyString(s, ms)
  def apply(s: String): ScrubyString = new ScrubyString(s, Map())
}
