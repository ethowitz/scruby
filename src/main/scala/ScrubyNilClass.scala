package scruby

object ScrubyNilClass extends ScrubyObject('NilClass, Map()) {
  override def toString: String = "nil"
}

