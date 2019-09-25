package scruby

class ScrubyString(val s: String, klass: Symbol, ms: Map[Symbol, ScrubyMethod])
    extends ScrubyObject(klass, ms) {
  override def toString: String = s
}

object ScrubyString {
  def apply(s: String): ScrubyString = new ScrubyString(s, 'String, Map())
}
