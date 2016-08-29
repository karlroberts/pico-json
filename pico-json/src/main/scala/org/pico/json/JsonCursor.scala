package org.pico.json

import scala.annotation.tailrec

case class JsonCursor(
    text: Array[Byte],
    ibs: BitVector,
    bps: BitVector,
    rank: Int)

object JsonCursor {
  implicit val balancedParens_Cursor = new BalancedParens[JsonCursor] {
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

    override def nextSibling(cursor: JsonCursor): Option[JsonCursor] = {
      if (cursor.bps(cursor.rank)) {
        @tailrec
        def go(rank: Int, depth: Int): Option[JsonCursor] = {
          if (rank < cursor.bps.length) {
            if (cursor.bps(rank)) {
              if (depth == 0) {
                Some(cursor.copy(rank = rank))
              } else {
                go(rank + 1, depth + 1)
              }
            } else {
              go(rank + 1, depth - 1)
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
  }
}