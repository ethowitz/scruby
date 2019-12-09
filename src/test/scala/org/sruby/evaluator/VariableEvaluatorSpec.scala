package org.sruby.evaluator

import org.sruby.core._
import org.sruby.parser._
import org.sruby.SRubySpec

class VariableEvaluatorSpec extends SRubySpec {
  "VariableEvaluator" should {
    "be EvaluatorLike[VariableNode]" in {
      // Note: In the example below, EvaluatorLike[A] --> EvaluatorLike[_] due to type erasure
      VariableEvaluator shouldBe an [EvaluatorLike[VariableNode]]
    }
  }

  "VariableEvaluator#eval" when {
    val subject: PartialFunction[VariableNode, Evaluator.Evaluation] = VariableEvaluator.eval

    val initialState: EvalState = factory[EvalState]

    "given a LocalVarAssignmentNode" should {
      val name: Symbol = 'test_name
      val value: AST = TrueNode
      val node: LocalVarAssignmentNode = LocalVarAssignmentNode(name, value)

      """return an Evaluation with an EvalState equal to the initial EvalState with an added local 
          |variable consisting of the given name and evaluated value""".stripMargin in {
        val updatedVariableMap = factory[VariableMap] + (name -> RubyTrueClass)
        val finalState = initialState.copy(localVars = updatedVariableMap)

        subject.valueAt(node).run(initialState).value should equal((finalState, RubyTrueClass))
      }
    }

    //"given an IvarAssignmentNode" should {
      //val name: Symbol = 'test_name
      //val value: AST = TrueNode
      //val node: IvarAssignmentNode = IvarAssignmentNode(name, value)

      //"""return an Evaluation with an EvalState equal to the initial EvalState whose self has an 
          //|additional ivar consisting of the given name and evaluated value""".stripMargin in {
        //val updatedSelf = initialState.self.withIvar(name -> RubyTrueClass)
        //val finalState = initialState.copy(self = updatedSelf)

        //subject.valueAt(node).run(initialState).value should equal((finalState, RubyTrueClass))
      //}
    //}
  }
}
