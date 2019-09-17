package scruby

case class ScrubyMethod(name: Symbol, params: List[String], ts: List[SyntaxTree])

object ScrubyMethod {
  def invoke(method: ScrubyMethod, args: List[ScrubyObjectContainer]): ScrubyObject = {
    val bindings: Map[String, ScrubyObjectContainer] = (method.params zip args) toMap

    Evaluator(method.ts.map(SyntaxTree.withBoundVars(_, bindings)))
  }
}
