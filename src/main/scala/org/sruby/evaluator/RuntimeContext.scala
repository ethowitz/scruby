package org.sruby.evaluator

import org.sruby.core.SRubyObject

case class RuntimeContext private(
  val objectID: Int,
  val instanceMethods: Map[Symbol, RubyMethod],
  val classMethods: Map[Symbol, RubyMethod],
  val instanceVariables: Map[Symbol, SRubyObject]
)

object RuntimeContext {
  // Public members
  def apply(
    instanceMethods: Map[Symbol, RubyMethod],
    classMethods: Map[Symbol, RubyMethod],
    instanceVariables: Map[Symbol, SRubyObject]
  ): RuntimeContext = {
    RuntimeContext(
      randomNumberGenerator.nextInt(maxObjectID),
      instanceMethods,
      classMethods,
      instanceVariables
    )
  }

  // Private members
  private val randomNumberGenerator: scala.util.Random = scala.util.Random
  private val maxObjectID: Integer = 100000
}
