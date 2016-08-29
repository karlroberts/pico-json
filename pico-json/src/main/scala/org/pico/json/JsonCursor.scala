package org.pico.json

import org.pico.json.syntax.balancedParens._

case class JsonCursor(
    text: String,
    ibs: BitVector,
    bps: BitVector,
    rank: Int) {
  def nav: Option[JsonCursor] = Some(this)

  def char: Char = {
    val result = for {
      i <- ibs.select(rank / 2 + 1)
      start = if (rank % 2 == 0) i - 1 else i
    } yield text(start)

    result getOrElse '\u0000'
  }

  def subtext: String = {
    val result = for {
      i <- ibs.select(rank / 2 + 1)
      start = if (rank % 2 == 0) i - 1 else i
      c <- Option(this).findClose
      j <- ibs.select(c.rank / 2 + 1)
      end = if (rank % 2 == 0) j else j - 1
    } yield text.substring(start, end)

    result getOrElse ""
  }
}
