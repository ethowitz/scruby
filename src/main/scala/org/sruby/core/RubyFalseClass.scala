package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

case class RubyFalseClass private(ctx: RuntimeContext) extends RubyFalseClassLike

object RubyFalseClass {
  def initialize: RubyFalseClass = {
    RubyFalseClass(RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject]))
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('FalseClass, instanceMethods, classMethods)
}
