package org.pico.json.syntax

import org.pico.json.{BalancedParens, JsonCursor}

import scala.annotation.tailrec

package object balancedParens {
  implicit class BalancedParensOps[A](val self: A) extends AnyVal {
    def fc(implicit ev: BalancedParens[A]): A = ev.firstChild(self)

    def ns(implicit ev: BalancedParens[A]): A = ev.nextSibling(self)

    def isOpen(implicit ev: BalancedParens[A]): Boolean = ev.isOpen(self)

    def next(implicit ev: BalancedParens[A]): A = ev.next(self)

    def findClose(implicit ev: BalancedParens[A]): A = ev.findClose(self)
  }

  implicit def balancedParens_Cursor = new BalancedParens[Option[JsonCursor]] {
    implicit def balancedParens_Cursor = this

    override def firstChild(maybeCursor: Option[JsonCursor]): Option[JsonCursor] = {
      for {
        cursor <- maybeCursor
        rank = cursor.rank
        result <- {
          if (cursor.bps(rank)) {
            if (cursor.bps(rank + 1)) {
              Some(cursor.copy(rank = rank + 1))
            } else {
              None
            }
          } else {
            None
          }
        }
      } yield result
    }

    override def findClose(maybeCursor: Option[JsonCursor]): Option[JsonCursor] = {
      for {
        cursor <- maybeCursor
        result <- {
          if (cursor.bps(cursor.rank)) {
            @tailrec
            def go(rank: Int, depth: Int): Option[JsonCursor] = {
              if (rank < cursor.bps.length) {
                if (cursor.bps(rank)) {
                  go(rank + 1, depth + 1)
                } else {
                  if (depth == 1) {
                    Some(cursor.copy(rank = rank))
                  } else {
                    go(rank + 1, depth - 1)
                  }
                }
              } else {
                None
              }
            }

            go(cursor.rank + 1, 1)
          } else {
            None
          }

        }
      } yield result
    }

    override def next(maybeCursor: Option[JsonCursor]): Option[JsonCursor] = {
      for {
        cursor <- maybeCursor
        result <- {
          val rank = cursor.rank + 1

          if (rank < cursor.bps.length) {
            Some(cursor.copy(rank = rank))
          } else {
            None
          }
        }
      } yield result
    }

    override def nextSibling(maybeCursor: Option[JsonCursor]): Option[JsonCursor] = {
      val n = maybeCursor.findClose.next

      if (n.isOpen) n else None
    }

    override def isOpen(maybeCursor: Option[JsonCursor]): Boolean = {
      {
        for {
          cursor <- maybeCursor
        } yield cursor.bps(cursor.rank)
      } getOrElse false
    }
  }}
