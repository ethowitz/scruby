package org.sruby.core

import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

final case class RubyClass private(
  val name: Symbol,
  val classInstanceMethods: Map[Symbol, RubyMethod],
  val classClassMethods: Map[Symbol, RubyMethod],
  val ctx: RuntimeContext
) extends RubyClassLike

object RubyClass extends SRubyObjectCompanion {
  def apply(
    name: Symbol,
    classInstanceMethods: Map[Symbol, RubyMethod],
    classClassMethods: Map[Symbol, RubyMethod]
  ): RubyClass =
    RubyClass(
      name,
      classInstanceMethods,
      classClassMethods,
      RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject])
    )

  def apply(name: Symbol): RubyClass =
    RubyClass(
      name,
      Map.empty[Symbol, RubyMethod],
      Map.empty[Symbol, RubyMethod],
      RuntimeContext(instanceMethods, classMethods, Map.empty[Symbol, SRubyObject])
    )


  val instanceMethods = Map.empty[Symbol, RubyMethod]
  val classMethods = Map.empty[Symbol, RubyMethod]

  val runtimeClass: RubyClass = RubyClass('Class, instanceMethods, classMethods)
}