package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

case class RubyTrueClass private(ctx: RuntimeContext) extends RubyTrueClassLike

object RubyTrueClass {
  def initialize: RubyTrueClass = {
    RubyTrueClass(RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject]))
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('TrueClass, instanceMethods, classMethods)
}
