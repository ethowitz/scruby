package org.sruby.evaluator

import org.sruby.core._

final case class Universe(
  val constants: ConstantTrie,
  val localVars: Map[Symbol, SRubyObject],
  val self: SRubyObject,
  val scope: List[Symbol]
)

object Universe {
  def initial: Universe = {
    val prepopulatedTrie = ConstantTrie.start.
      withConstant('BasicObject :: Nil, RubyBasicObject.runtimeClass).
      withConstant('Object :: Nil, RubyObject.runtimeClass).
      withConstant('Module :: Nil, RubyModule.runtimeClass).
      withConstant('Class :: Nil, RubyClass.runtimeClass).
      withConstant('FalseClass :: Nil, RubyFalseClass.runtimeClass).
      withConstant('TrueClass :: Nil, RubyTrueClass.runtimeClass).
      withConstant('NilClass :: Nil, RubyNilClass.runtimeClass).
      withConstant('String :: Nil, RubyString.runtimeClass).
      withConstant('Symbol :: Nil, RubySymbol.runtimeClass)

    Universe(prepopulatedTrie, Map.empty[Symbol, SRubyObject], RubyMain, Nil)
  }
}
