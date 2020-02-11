package org.sruby.core

import org.sruby.evaluator.PredefinedMethod

trait RubyBasicObjectLike extends SRubyObject {
  val predefinedInstanceMethods: Map[Symbol, PredefinedMethod] = Map.empty[Symbol, PredefinedMethod]
  val predefinedClassMethods: Map[Symbol, PredefinedMethod] = Map.empty[Symbol, PredefinedMethod]
}
