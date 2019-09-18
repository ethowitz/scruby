package scruby

object Evaluator {
  def apply(ts: List[SyntaxTree]) = evals(ts)

  def eval(t: SyntaxTree): ScrubyObject = t match {
    case KlassDef(name, es) => evalKlassDef(name, es)
    //case Invocation(receiver, message, args) => evalInvocation(receiver, message, args)
    case If(p, yes, no) => evalIf(p, yes, no)
    case Unless(p, statements) => evalUnless(p, statements)
    //case String_(s) => evalString(s)
    //case Symbol_(s) => ScrubySymbol(s)
    //case Integer_(n) => evalInteger(n)
    //case Float_(n) => evalFloat(n)
    case True => ScrubyTrueClass
    case False => ScrubyFalseClass
    case Nil_ => ScrubyNilClass
    case ScrubyObjectContainer(obj) => obj
  }

  def evals(ts: List[SyntaxTree]) = ts.map(eval).last

  def evalKlassDef(name: Symbol, ts: List[SyntaxTree]): ScrubyObject = {
    val methods = ts.foldLeft(Map[Symbol, ScrubyMethod]()) {
      (acc, t) => t match {
        case MethodDef(name, params, ts) => acc + (name -> ScrubyMethod(params, ts))
        case _ => acc
      }
    }

    ScrubyObject('Class, methods)
  }

  def evalIf(p: SyntaxTree, yes: List[SyntaxTree], no: List[SyntaxTree]): ScrubyObject = {
    eval(p) match {
      case ScrubyFalseClass | ScrubyNilClass => evals(no)
      case _ => evals(yes)
    }
  }

  def evalUnless(p: SyntaxTree, ts: List[SyntaxTree]): ScrubyObject = {
    eval(p) match {
      case ScrubyFalseClass | ScrubyNilClass => ScrubyNilClass
      case _ => evals(ts)
    }
  }
}
