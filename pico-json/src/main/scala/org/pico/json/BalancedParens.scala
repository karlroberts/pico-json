package org.pico.json

trait BalancedParens[A] {
  def firstChild(bp: A): A

  def nextSibling(bp: A): A

  def next(bp: A): A

  def isOpen(bp: A): Boolean

  def findClose(bp: A): A
}
