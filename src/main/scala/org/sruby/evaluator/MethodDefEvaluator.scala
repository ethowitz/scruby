package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object MethodDefEvaluator extends EvaluatorLike[MethodDefNode] {
  // Public members
  val eval: PartialFunction[MethodDefNode, Evaluation] = {
    case InstanceMethodDefNode(name, params, ts) => evalInstanceMethodDef(name, params, ts)
    case KlassMethodDefNode(name, params, ts) => evalKlassMethodDef(name, params, ts)
  }

  // Private members
  private def getKlass(name: List[Symbol]): Evaluation = State { s =>
    s.constants.get(name) match {
      case Some(klass) => (s, klass)
      case None => throw new Exception(s"class ${name.toString} does not exist")
    }
  }

  private def evalInstanceMethodDef(name: Symbol, params: List[Symbol], ts: List[AST]): Evaluation =
    for {
      scope <- State.inspect[Universe, List[Symbol]](_.scope)
      klass <- getKlass(scope)
      _ <- name match {
        case 'initialize => State.modify[Universe] { s =>
          val newKlass = klass.
            withKlassMethod('new -> RubyConstructor(scope, params, ts))
          val newSelf = s.self.
            withKlassMethod('new -> RubyConstructor(scope, params, ts))

          s.copy(constants = s.constants.withConstant(s.scope, newKlass), self = newSelf)
        }
        case _ => State.modify[Universe] { s =>
          val newKlass = klass.
            copy(instanceMethods = klass.instanceMethods + (name -> RubyMethod(params, ts)))
          val newSelf = s.self.withInstanceMethod(name -> RubyMethod(params, ts))

          s.copy(constants = s.constants.withConstant(s.scope, newKlass), self = newSelf)
        }
      }
    } yield RubySymbol(name)

  private def evalKlassMethodDef(name: Symbol, params: List[Symbol], ts: List[AST]): Evaluation =
    for {
      scope <- State.inspect[Universe, List[Symbol]](_.scope)
      klass <- getKlass(scope)
      _ <- State.modify[Universe] { s =>
        val newKlass = klass.withKlassMethod(name -> RubyMethod(params, ts))

        s.copy(constants = s.constants.withConstant(s.scope, newKlass), self = newKlass)
      }
    } yield RubySymbol(name)
  }
