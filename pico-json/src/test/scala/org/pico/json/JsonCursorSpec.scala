package org.pico.json

import org.pico.json.syntax.balancedParens._
import org.specs2.mutable.Specification

class JsonCursorSpec extends Specification {
  "JsonCursor" should {
    "supports balanced parens operations" in {
      val cursor: JsonCursor = JsonCursor(
        Array(),
        BitVector(Array()),
        BitVector.fromBalancedParens("((()())())"),
        0)

      cursor.rank must_=== 0

      cursor.nextSibling must_=== None

      val f = cursor.firstChild.get
      f.rank must_=== 1

      val ff = f.firstChild.get
      ff.rank must_== 2

      ff.firstChild must_=== None

      val ffn = ff.nextSibling.get
      ffn.rank must_== 4

      ffn.firstChild must_=== None
      ffn.nextSibling must_=== None

      val fn = f.nextSibling.get
      fn.rank must_=== 7

      fn.firstChild must_=== None
      fn.nextSibling must_=== None
    }
  }
}
