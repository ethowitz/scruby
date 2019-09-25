package scruby

class ScrubyObject(val klass: Symbol, val ms: Map[Symbol, ScrubyMethod] = Map()) {
  def methods: Map[Symbol, ScrubyMethod] = predefMethods ++ ms

  protected def predefMethods: Map[Symbol, ScrubyMethod] = {
    val nil: (Symbol, ScrubyMethod) = Symbol("nil?") -> ScrubyMethod(False)
    val _klass: (Symbol, ScrubyMethod) = 'class ->
      ScrubyMethod(ScrubyObjectContainer(ScrubyString(klass.name)))

    Map(nil, _klass)
  }
}

object ScrubyObject {
  def apply(klass: Symbol, ms: Map[Symbol, ScrubyMethod]): ScrubyObject =
    new ScrubyObject(klass, ms)

  def apply(klass: Symbol): ScrubyObject = apply(klass, Map())
}
