package org.pico.json

trait BalancedParens[A] {
  def firstChild(cursor: A): Option[A]

  def nextSibling(cursor: A): Option[A]
}
