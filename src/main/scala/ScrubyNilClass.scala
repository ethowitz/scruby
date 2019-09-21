package scruby

object ScrubyNilClass extends ScrubyObject('NilClass, Map(Symbol("nil?") -> ScrubyMethod(List(), True :: Nil))) {
  override def toString: String = "nil"

  //def methods: Map[Symbol, ScrubyMethod] = {
    //Map('nil -> nil)
  //}

  def nil: ScrubyMethod = ScrubyMethod(List(), True :: Nil)
}
