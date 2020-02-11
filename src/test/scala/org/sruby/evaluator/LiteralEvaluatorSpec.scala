package org.sruby.evaluator

import org.sruby.core._
import org.sruby.parser._
import org.sruby.SRubySpec

class LiteralEvaluatorSpec extends SRubySpec {
  "LiteralEvaluator" should {
    "be EvaluatorLike[LiteralNode]" in {
      // Note: In the example below, EvaluatorLike[A] --> EvaluatorLike[_] due to type erasure
      LiteralEvaluator shouldBe an [EvaluatorLike[LiteralNode]]
    }
  }

  "LiteralEvaluator#eval" when {
    val subject: PartialFunction[LiteralNode, Evaluation] = LiteralEvaluator.eval

    val initialState: Universe = factory[Universe]

    "given SelfNode" should {
      """return an Evaluation whose value is the current Universe's self with an unchanged 
          |Universe""".stripMargin in {
        subject.valueAt(SelfNode).run(initialState).value should
          equal((initialState, initialState.self))
      }
    }

    "given TrueNode" should {
      "return an Evaluation that has a value of RubyTrueClass with an unchanged Universe" in {
        subject.valueAt(TrueNode).run(initialState).value should
          equal((initialState, RubyTrueClass))
      }
    }

    "given FalseNode" should {
      "return an Evaluation that has a value of RubyFalseClass with an unchanged Universe" in {
        subject.valueAt(FalseNode).run(initialState).value should
          equal((initialState, RubyFalseClass))
      }
    }

    "given NilNode" should {
      "return an Evaluation that has a value of RubyNilClass with an unchanged Universe" in {
        subject.valueAt(NilNode).run(initialState).value should equal((initialState, RubyNilClass))
      }
    }
  }
}
