package org.pico.json

import org.pico.json.syntax.balancedParens._
import org.specs2.mutable.Specification

class JsonCursorSpec extends Specification {
  "JsonCursor" should {
    "supports balanced parens operations" in {
      val cursor: JsonCursor = JsonCursor(
        "",
        BitVector(Array()),
        BitVector.fromBalancedParens("((()())())"),
        0)

      cursor.rank must_=== 0

      cursor.nav.nextSibling must_=== None

      val f = cursor.nav.firstChild.get
      f.rank must_=== 1

      val ff = f.nav.firstChild.get
      ff.rank must_== 2

      ff.nav.firstChild must_=== None

      val ffn = ff.nav.nextSibling.get
      ffn.rank must_== 4

      ffn.nav.firstChild must_=== None
      ffn.nav.nextSibling must_=== None

      val fn = f.nav.nextSibling.get
      fn.rank must_=== 7

      fn.nav.firstChild must_=== None
      fn.nav.nextSibling must_=== None
    }

    "supports subtext operation" in {
      val cursor: JsonCursor = JsonIO.loadWithIndex(
        """{"a": 1, "b": 2}""")

      cursor.subtext must_=== """{"a": 1, "b": 2}"""
      cursor.nav.firstChild.get.subtext must_=== """ "a" """.trim
      cursor.nav.firstChild.nextSibling.get.subtext must_=== """ "a" """.trim
    }
  }
}
