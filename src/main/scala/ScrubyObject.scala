package scruby

case class ScrubyObject(klass: Symbol, ms: Map[Symbol, ScrubyMethod])

object ScrubyObject {
  def nil: ScrubyObject = ScrubyFalseClass
}
