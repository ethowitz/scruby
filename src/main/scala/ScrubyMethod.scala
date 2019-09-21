package scruby

case class ScrubyMethod(params: List[Symbol], ts: List[SyntaxTree])

object ScrubyMethod {
  def invoke(method: ScrubyMethod, args: List[ScrubyObject]): ScrubyObject = {
    val bindings: Map[Symbol, ScrubyObject] = (method.params zip args) toMap

    Evaluator(method.ts.map(SyntaxTree.withBoundVars(_, bindings)))
  }
}
