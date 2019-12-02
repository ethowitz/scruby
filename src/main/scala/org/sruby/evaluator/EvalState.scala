package org.sruby.evaluator

import org.sruby.core.RubyObject

// scope will eventually be a RubyConstant
final case class EvalState(
  val klasses: KlassMap, val localVars: VariableMap, val self: RubyObject, val scope: Symbol
)

object EvalState {
  def start: EvalState = EvalState(KlassMap.empty, VariableMap.empty, RubyMain, 'main)
}
