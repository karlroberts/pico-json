package org.pico.json

import org.pico.json.syntax.balancedParens._

case class JsonCursor(
    text: String,
    ibs: BitVector,
    bps: BitVector,
    rank: Int) {
  def nav: Option[JsonCursor] = Some(this)

  def subtext: String = {
    val result = for {
      i <- ibs.select(rank / 2)
      start <- ibs.select(i + 1)
      c <- Option(this).findClose
      j <- ibs.select(c.rank)
      end = j
    } yield text.substring(start, end)

    result getOrElse ""
  }
}
