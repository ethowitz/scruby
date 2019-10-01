package scruby

object ScrubyFalseClass extends ScrubyObject('FalseClass, None, Map()) {
  override def toString: String = "false"
}
