package org.pico.json

trait BalancedParens[A] {
  def firstChild(bp: A): Option[A]

  def nextSibling(bp: A): Option[A]

  def next(bp: A): Option[A]

  def isOpen(bp: A): Boolean

  def findClose(bp: A): Option[A]
}
