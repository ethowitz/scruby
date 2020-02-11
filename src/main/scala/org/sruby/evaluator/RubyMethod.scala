package org.sruby.evaluator

import cats.data.State
import org.sruby.parser.AST
import org.sruby.core.SRubyObject

trait RubyMethod {
  def invoke(args: State[Universe, Seq[SRubyObject]]): State[Universe, SRubyObject]
}

object RubyMethod {
  def apply(arg: AST): RubyMethod = RuntimeDefinedMethod(Seq.empty[Symbol], List[AST](arg))
  def apply(params: Seq[Symbol], ts: List[AST]): RubyMethod = RuntimeDefinedMethod(params, ts)
}
