package org.sruby.exceptions

object RuntimeError {
  def apply(err: String, stack: List[String]): String = err + "\n" + stackMessage(stack)
  def stackMessage(stack: List[String]): String = stack.mkString("\n")
}
