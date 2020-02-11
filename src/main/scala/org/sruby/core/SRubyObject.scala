package org.sruby.core

import org.sruby.evaluator.PredefinedMethod
import org.sruby.evaluator.RubyMethod
import org.sruby.evaluator.RuntimeContext

trait SRubyObject {
  val predefinedInstanceMethods: Map[Symbol, PredefinedMethod]
  val predefinedClassMethods: Map[Symbol, PredefinedMethod]

  val methods: Map[Symbol, RubyMethod] = predefinedInstanceMethods ++ ctx.instanceMethods
}

trait SRubyObjectCompanion {
  val runtimeClass: RubyClass
}
