package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

case class RubyObject private(ctx: RuntimeContext) extends RubyObjectLike

object RubyObject extends SRubyObjectCompanion {
  def initialize: RubyObject = {
    RubyObject(RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject]))
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('Object, instanceMethods, classMethods)
}
