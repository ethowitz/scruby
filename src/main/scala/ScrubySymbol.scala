package scruby

class ScrubySymbol(val s: Symbol, klass: Symbol, ms: Map[Symbol, ScrubyMethod])
    extends ScrubyObject(klass, ms) {
  override def toString: String = s.toString.replace("'", "")
}

object ScrubySymbol {
  def apply(s: Symbol): ScrubySymbol = new ScrubySymbol(s, 'Symbol, Map())
}
