package scruby

case class ScrubyMethod(params: List[Symbol], ts: List[SyntaxTree])

object ScrubyMethod {
  def invoke(method: ScrubyMethod, args: List[ScrubyObjectContainer]): ScrubyObject = {
    val bindings: Map[Symbol, ScrubyObjectContainer] = (method.params zip args) toMap

    Evaluator(method.ts.map(SyntaxTree.withBoundVars(_, bindings)))
  }
}
