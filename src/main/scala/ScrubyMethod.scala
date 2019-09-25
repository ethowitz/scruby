package scruby

case class ScrubyMethod(params: List[Symbol], ts: List[SyntaxTree])

object ScrubyMethod {
  def apply(arg: SyntaxTree): ScrubyMethod = ScrubyMethod(List(), List(arg))

  def invoke(method: ScrubyMethod, args: List[ScrubyObject]): ScrubyObject = {
    val bindings: Map[Symbol, ScrubyObject] = (method.params zip args) toMap

    Evaluator(method.ts.map(SyntaxTree.withBoundVars(_, bindings)))
  }
}
