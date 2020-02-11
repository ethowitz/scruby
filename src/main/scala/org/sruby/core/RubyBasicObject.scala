package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

case class RubyBasicObject private(ctx: RuntimeContext) extends RubyBasicObjectLike

object RubyBasicObject extends SRubyObjectCompanion {
  def apply: RubyBasicObject = {
    RubyBasicObject(RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject]))
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('BasicObject, instanceMethods, classMethods)
}
