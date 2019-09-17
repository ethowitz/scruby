package scruby

object Evaluator {
  def apply(ts: List[SyntaxTree]) = evals(ts)

  private def eval(t: SyntaxTree): ScrubyObject = t match {
    //case KlassDef(name, es) => evalKlassDef(name, es)
    //case MethodDef(name, statements) => evalMethodDef(name, statements)
    //case Invocation(receiver, message, args) => evalInvocation(receiver, message, args)
    case If(p, yes, no) => evalIf(p, yes, no)
    case Unless(p, statements) => evalUnless(p, statements)
    //case Identifier(name) => evalIdentifier(name)
    //case String_(s) => evalString(s)
    //case Symbol_(s) => evalSymbol(s)
    //case Integer_(n) => evalInteger(n)
    //case Float_(n) => evalFloat(n)
    case True => evalTrue
    case False => evalFalse
    case Nil_ => evalNil
    case ScrubyObjectContainer(obj) => obj
  }

  private def evals(ts: List[SyntaxTree]) = ts.map(eval).last

  private def evalNil = ScrubyNilClass
  private def evalTrue = ScrubyTrueClass
  private def evalFalse = ScrubyFalseClass

  private def evalIf(p: SyntaxTree, yes: List[SyntaxTree], no: List[SyntaxTree]): ScrubyObject = {
    eval(p) match {
      case ScrubyFalseClass | ScrubyNilClass => evals(no)
      case _ => evals(yes)
    }
  }

  private def evalUnless(p: SyntaxTree, ts: List[SyntaxTree]): ScrubyObject = {
    eval(p) match {
      case ScrubyFalseClass | ScrubyNilClass => ScrubyNilClass
      case _ => evals(ts)
    }
  }
}
