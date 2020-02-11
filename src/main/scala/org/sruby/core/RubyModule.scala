package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

final case class RubyModule private(
  val name: Symbol,
  val moduleInstanceMethods: Map[Symbol, RubyMethod],
  val moduleClassMethods: Map[Symbol, RubyMethod],
  val ctx: RuntimeContext
) extends RubyModuleLike

object RubyModule extends SRubyObjectCompanion {
  def apply(
     name: Symbol,
     classInstanceMethods: Map[Symbol, RubyMethod],
     classModuleMethods: Map[Symbol, RubyMethod]
   ): RubyModule = {
    RubyModule(
      name,
      classInstanceMethods,
      classModuleMethods,
      RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject])
    )
  }

  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('Module, instanceMethods, classMethods)
}
