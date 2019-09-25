package scruby

object ScrubyTrueClass extends ScrubyObject('TrueClass) {
  override def toString: String = "true"
}
