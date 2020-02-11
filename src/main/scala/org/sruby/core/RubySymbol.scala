package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

case class RubySymbol private(val s: Symbol, ctx: RuntimeContext) extends RubySymbolLike

object RubySymbol extends SRubyObjectCompanion {
  def apply(s: Symbol): RubySymbol = {
    RubySymbol(s, RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject]))
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('Symbol, instanceMethods, classMethods)
}
