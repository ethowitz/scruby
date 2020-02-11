package org.sruby.evaluator

import org.sruby.core.SRubyObject
import scala.annotation.tailrec

final case class ConstantTrie private(
  val obj: SRubyObject, val subtrees: Map[Symbol, ConstantTrie]
) {
  /* Public members */
  @tailrec
  def withConstant(prefix: List[Symbol], obj: SRubyObject): ConstantTrie = prefix match {
    case Nil => ConstantTrie(obj, Map.empty[Symbol, ConstantTrie])
    case c :: cs => subtrees get c match {
      case None => throw new Exception("prefix contains non-existent nodes")
      case Some(child) => copy(subtrees = subtrees + (c -> child.withConstant(cs, obj)))
    }
  }

  @tailrec
  def withoutConstant(prefix: List[Symbol]): ConstantTrie = prefix match {
    case c :: Nil => if (subtrees contains c) {
      copy(subtrees = subtrees - c)
    } else {
      throw new Exception("prefix contains non-existent nodes")
    }
    case c :: cs => subtrees get c match {
      case None => throw new Exception("prefix contains non-existent nodes")
      case Some(child) => child.withoutConstant(cs)
    }
  }

  def get(prefix: List[Symbol]): Option[SRubyObject] = prefix match {
    case Nil => Some(obj)
    case key :: keys => subtrees.get(key).flatMap(_.get(keys))
  }
}

object ConstantTrie {
  def start: ConstantTrie = ConstantTrie(RubyMain, Map.empty[Symbol, ConstantTrie])
}
