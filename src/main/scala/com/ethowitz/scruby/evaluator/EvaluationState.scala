package com.ethowitz.scruby.evaluator

import com.ethowitz.scruby.core.ScrubyNilClass
import com.ethowitz.scruby.core.ScrubyObject

case class EvaluationState(value: ScrubyObject, klasses: KlassMap, localVars: VariableMap, self: Option[ScrubyObject]) {
  def withSelf(s: ScrubyObject): EvaluationState =
    EvaluationState(value, klasses, localVars, Some(s))

  def withValue(v: ScrubyObject): EvaluationState = EvaluationState(v, klasses, localVars, self)

  def withLocalVars(vs: VariableMap): EvaluationState = EvaluationState(value, klasses, vs, self)
}

object EvaluationState {
  def start: EvaluationState =
    EvaluationState(ScrubyNilClass, KlassMap.empty, VariableMap.empty, None)
}

