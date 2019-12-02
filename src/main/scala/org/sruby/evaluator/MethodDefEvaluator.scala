package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object MethodDefEvaluator extends EvaluatorLike {
  // Public members
  def eval: PartialFunction[AST, Evaluator.Evaluation] = {
    case InstanceMethodDefNode(name, params, ts) => evalInstanceMethodDef(name, params, ts)
    case KlassMethodDefNode(name, params, ts) => evalKlassMethodDef(name, params, ts)
  }

  // Private members
  private def getKlass(name: Symbol): Evaluator.Evaluation = State { s =>
    val klass =
      s.klasses.get(name).getOrElse(throw new Exception(s"class ${name.toString} does not exist"))

    (s, klass)
  }

  private def evalInstanceMethodDef(
    name: Symbol, params: List[Symbol], ts: List[AST]
  ): Evaluator.Evaluation = for {
    scope <- State.inspect[EvalState, Symbol](_.scope)
    klass <- getKlass(scope)
    _ <- name match {
      case 'initialize => State.modify[EvalState] { s =>
        val newKlass = klass.withKlassMethod('new -> RubyConstructor(scope, params, ts))
        val newSelf = s.self.withKlassMethod('new -> RubyConstructor(scope, params, ts))

        s.copy(klasses = s.klasses + (s.scope -> newKlass), self = newSelf)
      }
      case _ => State.modify[EvalState] { s =>
        val newKlass = klass.withInstanceMethod(name -> RubyMethod(params, ts))
        val newSelf = s.self.withInstanceMethod(name -> RubyMethod(params, ts))

        s.copy(klasses = s.klasses + (s.scope -> newKlass), self = newSelf)
      }
    }
  } yield RubySymbol(name)

  private def evalKlassMethodDef(
    name: Symbol, params: List[Symbol], ts: List[AST]
  ): Evaluator.Evaluation = for {
    scope <- State.inspect[EvalState, Symbol](_.scope)
    klass <- getKlass(scope)
    _ <- State.modify[EvalState] { s =>
      val newKlass = klass.withKlassMethod(name -> RubyMethod(params, ts))

      s.copy(klasses = s.klasses + (s.scope -> newKlass), self = newKlass)
    }
  } yield RubySymbol(name)
}
