package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.core.RubyNilClass
import com.ethowitz.sruby.core.RubyObject

final case class EvaluationState(
  value: RubyObject,
  klasses: KlassMap,
  localVars: VariableMap,
  self: RubyObject
) {
  def withSelf(s: RubyObject): EvaluationState = EvaluationState(value, klasses, localVars, s)

  def withValue(v: RubyObject): EvaluationState = EvaluationState(v, klasses, localVars, self)

  def withLocalVars(ls: VariableMap): EvaluationState = EvaluationState(value, klasses, ls, self)
}

object EvaluationState {
  def start: EvaluationState =
    EvaluationState(RubyNilClass, KlassMap.empty, VariableMap.empty, RubyMain)
}

