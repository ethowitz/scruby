package org.sruby.evaluator

import org.sruby.parser.AST

trait EvaluatorLike {
  def eval: PartialFunction[AST, Evaluator.Evaluation]
}

