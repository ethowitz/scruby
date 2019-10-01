package scruby

object ScrubyNilClass extends ScrubyObject('NilClass, None, Map()) {
  override def toString: String = "nil"

  override def predefMethods: Map[Symbol, ScrubyMethod] =
    super.predefMethods + (Symbol("nil?") -> ScrubyMethod(True))
}
