package org.sruby.core

import org.sruby.evaluator.PredefinedMethod
import org.sruby.evaluator.RubyMethod

trait RubyClassLike extends RubyModuleLike {
  override val predefinedInstanceMethods: Map[Symbol, PredefinedMethod] =
    super.predefinedInstanceMethods ++ Map.empty[Symbol, PredefinedMethod]

  override val predefinedClassMethods: Map[Symbol, PredefinedMethod] =
    super.predefinedClassMethods ++ Map.empty[Symbol, PredefinedMethod]

  override val methods: Map[Symbol, RubyMethod] = predefinedClassMethods ++ ctx.classMethods
}
