package com.ethowitz.scruby.core

object ScrubyFalseClass extends ScrubyObject('FalseClass, None, Map()) {
  override def toString: String = "false"
}
