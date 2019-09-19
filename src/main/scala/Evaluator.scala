package scruby

object Evaluator {
  def apply(ts: List[SyntaxTree]) = evals(ts, evalKlassDefs(ts) + ('Unbound -> evalKlassDef(ts)))

  def evalKlassDefs(ts: List[SyntaxTree]): Map[Symbol, ScrubyObject] = {
    ts.foldLeft(Map[Symbol, ScrubyObject]()) { (acc, t) =>
      t match {
        case KlassDef(name, ts) => acc + (name -> evalKlassDef(ts))
        case _ => acc
      }
    }
  }

  def evals(ts: List[SyntaxTree], ks: Map[Symbol, ScrubyObject]): ScrubyObject = {
    def eval(t: SyntaxTree): ScrubyObject = t match {
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

    def evalIf(p: SyntaxTree, yeses: List[SyntaxTree], nos: List[SyntaxTree]): ScrubyObject = {
      eval(p) match {
        case ScrubyFalseClass | ScrubyNilClass => evals(nos, ks)
        case _ => evals(yeses, ks)
      }
    }

    def evalUnless(p: SyntaxTree, ts: List[SyntaxTree]): ScrubyObject = {
      eval(p) match {
        case ScrubyFalseClass | ScrubyNilClass => ScrubyNilClass
        case _ => evals(ts, ks)
      }
    }

    ts.flatMap { (t) =>
      t match {
        case KlassDef(_, _) => None
        case MethodDef(_, _, _) => None
        case t => Some(eval(t))
      }
    }.lastOption.getOrElse(ScrubyNilClass)
  }

  def evalKlassDef(ts: List[SyntaxTree]): ScrubyObject = {
    val methods = ts.foldLeft(Map[Symbol, ScrubyMethod]()) {
      (acc, t) => t match {
        case MethodDef(name, params, ts) => acc + (name -> ScrubyMethod(params, ts))
        case t => acc
      }
    }

    ScrubyObject('Class, methods)
  }
}
