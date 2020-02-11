package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

case class RubyNilClass private(ctx: RuntimeContext) extends RubyNilClassLike

object RubyNilClass {
  def initialize: RubyNilClass = {
    RubyNilClass(RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject]))
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('NilClass, instanceMethods, classMethods)
}
