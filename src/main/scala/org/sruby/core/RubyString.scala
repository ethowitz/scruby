package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

case class RubyString private(val s: String, ctx: RuntimeContext) extends RubyStringLike

object RubyString extends SRubyObjectCompanion {
  def apply(s: String): RubyString = {
    RubyString(s, RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject]))
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('String, instanceMethods, classMethods)
}
