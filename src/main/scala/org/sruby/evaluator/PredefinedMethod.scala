package org.sruby.evaluator

import cats.data.State
import org.sruby.core.SRubyObject

sealed trait PredefinedMethod extends RubyMethod

object PredefinedMethod {
  def apply(f: () => SRubyObject): PredefinedMethod = ParamlessPredefinedMethod(f)
  def apply(f: Seq[SRubyObject] => SRubyObject): PredefinedMethod = PredefinedMethodWithParams(f)
}

final case class PredefinedMethodWithParams(val f: Seq[_ <: SRubyObject] => SRubyObject)
    extends PredefinedMethod {
  def invoke(args: State[Universe, Seq[SRubyObject]]): State[Universe, SRubyObject] = args.map(f(_))
}

final case class ParamlessPredefinedMethod(val f: () => SRubyObject) extends PredefinedMethod {
  def invoke(args: State[Universe, Seq[SRubyObject]]): State[Universe, SRubyObject] =
    args.map { _ => f() }
}

