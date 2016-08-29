package org.pico.json.syntax

import org.pico.json.BalancedParens

package object balancedParens {
  implicit class BalancedParensOps[A](val self: A) extends AnyVal {
    def firstChild(implicit ev: BalancedParens[A]): Option[A] = ev.firstChild(self)

    def nextSibling(implicit ev: BalancedParens[A]): Option[A] = ev.nextSibling(self)

    def isOpen(implicit ev: BalancedParens[A]): Boolean = ev.isOpen(self)

    def next(implicit ev: BalancedParens[A]): Option[A] = ev.next(self)

    def findClose(implicit ev: BalancedParens[A]): Option[A] = ev.findClose(self)
  }
}
