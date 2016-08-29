package org.pico.json

import org.pico.json.syntax.balancedParens._

import scala.annotation.tailrec

case class JsonCursor(
    text: Array[Byte],
    ibs: BitVector,
    bps: BitVector,
    rank: Int)

object JsonCursor {
  implicit val balancedParens_Cursor = new BalancedParens[JsonCursor] {
    implicit val selfImplicit = this

    override def firstChild(cursor: JsonCursor): Option[JsonCursor] = {
      val rank = cursor.rank

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

    override def findClose(cursor: JsonCursor): Option[JsonCursor] = {
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

    override def next(cursor: JsonCursor): Option[JsonCursor] = {
      val rank = cursor.rank + 1
      val bpLength = cursor.bps.length

      if (rank < cursor.bps.length) {
        Some(cursor.copy(rank = rank))
      } else {
        None
      }
    }

    override def nextSibling(cursor: JsonCursor): Option[JsonCursor] = {
      for {
        c <- cursor.findClose
        n <- c.next
        ns <- if (n.isOpen) Some(n) else None
      } yield ns
    }

    override def isOpen(bp: JsonCursor): Boolean = bp.bps(bp.rank)
  }
}