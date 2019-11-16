package com.ethowitz.sruby.evaluator

import com.ethowitz.sruby.core.RubyObject

final case class EvalState(klasses: KlassMap, localVars: VariableMap, self: RubyObject) {
  def addIvarOnSelf(mapping: (Symbol, RubyObject)): EvalState =
    EvalState(klasses, localVars, self.withIvar(mapping))

  def addMethodOnSelf(mapping: (Symbol, RubyMethod)): EvalState =
    EvalState(klasses, localVars, self.withMethod(mapping))

  def addLocalVar(mapping: (Symbol, RubyObject)): EvalState =
    EvalState(klasses, localVars + mapping, self)

  def addKlass(mapping: (Symbol, RubyObject)): EvalState =
    EvalState(klasses + mapping, localVars, self)

  def setLocalVars(newLocalVars: VariableMap): EvalState = EvalState(klasses, newLocalVars, self)

  def setSelf(newSelf: RubyObject): EvalState = EvalState(klasses, localVars, newSelf)
}

object EvalState {
  def start: EvalState = EvalState(KlassMap.empty, VariableMap.empty, RubyMain)
}

