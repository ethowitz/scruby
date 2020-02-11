package org.sruby.core

import org.sruby.evaluator.PredefinedMethod

trait RubyObjectLike extends RubyBasicObjectLike {
//  val nil: RubyFalseClassLike = RubyFalse
override val predefinedInstanceMethods: Map[Symbol, PredefinedMethod] =
  super.predefinedInstanceMethods ++ Map.empty[Symbol, PredefinedMethod]

  override val predefinedClassMethods: Map[Symbol, PredefinedMethod] =
    super.predefinedClassMethods ++ Map.empty[Symbol, PredefinedMethod]
}

