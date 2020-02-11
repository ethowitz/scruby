package org.sruby

import org.sruby.core._
import org.sruby.evaluator._

trait Factories {
  // Public members
  def factory[A](implicit f: () => A): A = f()

  // Protected members
  protected implicit val evalState: () => Universe =
    () => Universe(klassMap(), variableMap(), rubyObject(), 'TestScope)

  protected implicit val klassMap: () => KlassMap = () => KlassMap.empty

  protected implicit val rubyObject: () => RubyObject = () => RubyObject('TestClass)

  protected implicit val variableMap: () => VariableMap = () => VariableMap.empty
}
