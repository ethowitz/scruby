package org.sruby.evaluator

import cats.data.State
import org.sruby.core._
import org.sruby.parser._

object InvocationEvaluator extends EvaluatorLike {
  // Public members
  def eval: PartialFunction[AST, Evaluator.Evaluation] = {
    case InvocationWithReceiverNode(recvr, msg, args) =>
      evalInvocationWithReceiver(recvr, msg, args)
    case InvocationWithImplicitReceiverNode(msg, args) =>
      evalInvocationWithImplicitReceiver(msg, args)
  }

  // Private members
  private def evalArgs(args: List[AST]): State[EvalState, Seq[RubyObject]] = {
    val initialValue = State.pure[EvalState, Seq[RubyObject]](Seq.empty[RubyObject])

    args.foldLeft(initialValue) { (acc, arg) =>
      Evaluator.eval(arg).flatMap { evaldArg => acc.map(_ ++ Seq(evaldArg)) }
    }
  }

  private def evalInvocationWithImplicitReceiver(
    msg: Symbol, args: List[AST]
  ): Evaluator.Evaluation = for {
    EvalState(_, prevLocalVars, prevSelf, prevScope) <- State.get[EvalState]
    result <- prevLocalVars get msg match {
      case Some(obj) => State.pure[EvalState, RubyObject](obj)
      case None => prevSelf.methods get msg match {
        case Some(method) => method.invoke(evalArgs(args))
        case None => throw new Exception(s"undefined local variable or method `${msg.toString}'")
      }
    }
    _ <- State.modify[EvalState](_.copy(localVars = prevLocalVars))
  } yield result

  private def evalInvocationWithReceiver(
    recvr: AST, msg: Symbol, args: List[AST]
  ): Evaluator.Evaluation = for {
    evaldReceiver <- Evaluator.eval(recvr)
    EvalState(prevKlasses, prevLocalVars, prevSelf, prevScope) <- State.get[EvalState]
    result <- evaldReceiver.methods.get(msg) match {
      case Some(method) => for {
        evaldArgs <- evalArgs(args)
        _ <- State.modify[EvalState](_.copy(self = evaldReceiver, scope = evaldReceiver.klass))
        result <- method.invoke(State.pure(evaldArgs))
        EvalState(_, _, newSelf, _) <- State.get[EvalState]
        _ <- recvr match {
          case InvocationWithImplicitReceiverNode(name, _) => State.modify[EvalState] { s =>
            s.copy(localVars = prevLocalVars + (name -> newSelf), self = prevSelf,
              scope = prevScope)
          }
          case IvarIdentifierNode(name) => State.modify[EvalState] { s =>
            s.copy(localVars = prevLocalVars, self = s.self.withIvar(name -> newSelf),
              scope = prevScope)
          }
          case ConstantNode(name) => State.modify[EvalState] { s =>
            s.copy(klasses = prevKlasses + (name -> newSelf), localVars = prevLocalVars,
              self = prevSelf, scope = prevScope)
          }
          case _ => State.modify[EvalState](_.copy(localVars = prevLocalVars, self = prevSelf,
            scope = prevScope))
        }
      } yield result
      case None =>
        throw new Exception(s"undefined method `${msg.toString}' for `${evaldReceiver}'")
    }
  } yield result
}
