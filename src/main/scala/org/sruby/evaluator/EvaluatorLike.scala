package org.sruby.evaluator

import org.sruby.parser.AST

trait EvaluatorLike[A <: AST] {
  val eval: PartialFunction[A, Evaluator.Evaluation]
}

