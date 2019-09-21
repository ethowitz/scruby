package scruby

import scala.io.Source

object Scruby {
  def main(args: Array[String]): Unit = {
    val fileContents = Source.fromFile(args.head).getLines mkString "\n"

    Compiler(fileContents) match {
      case Left(error) => println(error)
      case Right(syntaxTree) => println(Evaluator(syntaxTree))
    }
  }
}
