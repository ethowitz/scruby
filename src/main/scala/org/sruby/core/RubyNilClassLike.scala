package org.sruby.core

import org.sruby.evaluator.PredefinedMethod

trait RubyNilClassLike extends RubyObjectLike {
  override val predefinedInstanceMethods: Map[Symbol, PredefinedMethod] =
    super.predefinedInstanceMethods ++ Map.empty[Symbol, PredefinedMethod]

  override val predefinedClassMethods: Map[Symbol, PredefinedMethod] =
    super.predefinedClassMethods ++ Map.empty[Symbol, PredefinedMethod]
}
