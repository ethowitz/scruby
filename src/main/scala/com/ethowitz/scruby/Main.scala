package com.ethowitz.scruby

import scala.io.Source
import compiler.Compiler
import evaluator.Evaluator

object Scruby {
  def main(args: Array[String]): Unit = {
    val fileContents = Source.fromFile(args.head).getLines mkString "\n"

    Compiler(fileContents) match {
      case Left(error) => println(error)
      case Right(syntaxTree) => println(Evaluator(syntaxTree))
    }
  }
}
