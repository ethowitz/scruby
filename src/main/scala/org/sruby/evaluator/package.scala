package org.sruby

import cats.data.State
import org.sruby.core._
import org.sruby.core.RubyNilClass
import org.sruby.core.RubyObject
import org.sruby.core.RubyTrueClass
import org.sruby.core.SRubyObject

package object evaluator {
  type Evaluation = State[Universe, SRubyObject]

  val RubyTrue: SRubyObject = RubyTrueClass.initialize
  val RubyFalse: SRubyObject = RubyFalseClass.initialize
  val RubyNil: SRubyObject = RubyNilClass.initialize

  val RubyMain: SRubyObject = RubyObject.initialize
}
