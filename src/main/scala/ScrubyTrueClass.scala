package scruby

object ScrubyTrueClass extends ScrubyObject('TrueClass, None, Map()) {
  override def toString: String = "true"
}
