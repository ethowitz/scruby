package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object InvocationEvaluator extends EvaluatorLike[InvocationNode] {
  // Public members
  val eval: PartialFunction[InvocationNode, Evaluation] = {
    case InvocationWithReceiverNode(recvr, msg, args) =>
      evalInvocationWithReceiver(recvr, msg, args)
    case InvocationWithImplicitReceiverNode(msg, args) =>
      evalInvocationWithImplicitReceiver(msg, args)
  }

  // Private members
  private def evalArgs(args: List[AST]): State[Universe, Seq[SRubyObject]] = {
    val initialValue = State.pure[Universe, Seq[SRubyObject]](Seq.empty[SRubyObject])

    args.foldLeft(initialValue) { (acc, arg) =>
      Evaluator.eval(arg).flatMap { evaluatedArg => acc.map(_ ++ Seq(evaluatedArg)) }
    }
  }

  private def evalInvocationWithImplicitReceiver(msg: Symbol, args: List[AST]): Evaluation = for {
    Universe(_, prevLocalVars, prevSelf, prevScope) <- State.get[Universe]
    result <- prevLocalVars get msg match {
      case Some(obj) => State.pure[Universe, SRubyObject](obj)
      case None => prevSelf.methods get msg match {
        case Some(method) => method.invoke(evalArgs(args))
        case None => throw new Exception(s"undefined local variable or method `${msg.toString}'")
      }
    }
    _ <- State.modify[Universe](_.copy(localVars = prevLocalVars))
  } yield result

  // TODO TODO TODO replace the withBlah methods with lenses
  // TODO TODO TODO use package objects to remove #eval and make *Evaluators directly composable
  // http://julien-truffaut.github.io/Monocle/optics/lens.html
  private def evalInvocationWithReceiver(receiver: AST, msg: Symbol, args: List[AST]): Evaluation =
    for {
      evaluatedReceiver <- Evaluator.eval(receiver)
      Universe(prevConstants, prevLocalVars, prevSelf, prevScope) <- State.get[Universe]
      result <- evaluatedReceiver.methods get msg match {
        case Some(method) => for {
          evaluatedArgs <- evalArgs(args)
          _ <- State.
            modify[Universe](_.copy(self = evaluatedReceiver, scope = evaluatedReceiver.klass))
          result <- method.invoke(State.pure(evaluatedArgs))
          Universe(_, _, newSelf, _) <- State.get[Universe]
          _ <- receiver match {
            case InvocationWithImplicitReceiverNode(name, _) => State.modify[Universe] { s =>
              s.copy(localVars = prevLocalVars + (name -> newSelf), self = prevSelf,
                scope = prevScope)
            }
            case IvarIdentifierNode(name) => State.modify[Universe] { s =>
              s.copy(localVars = prevLocalVars, self = s.self.withIvar(name -> newSelf),
                scope = prevScope)
            }
            case ConstantNode(name) => State.modify[Universe] { s =>
              s.copy(constants = prevConstants.withConstant(name :: Nil, newSelf),
                localVars = prevLocalVars, self = prevSelf, scope = prevScope)
            }
            case _ => State.modify[Universe](_.copy(localVars = prevLocalVars, self = prevSelf,
              scope = prevScope))
          }
        } yield result
        case None =>
          throw new Exception(s"undefined method `${msg.toString}' for `${evaluatedReceiver}'")
      }
    } yield result
}
