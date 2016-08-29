package org.pico.json

import org.pico.json.syntax.balancedParens._
import org.specs2.mutable.Specification

class JsonCursorSpec extends Specification {
  "JsonCursor" should {
    "supports balanced parens operations" in {
      val o1: JsonCursor = JsonCursor(
        "",
        BitVector(Array()),
        BitVector.fromBalancedParens("((()())())"),
        0)

      o1.rank must_=== 0

      o1.nav.nextSibling must_=== None

      val o2 = o1.nav.firstChild.get
      o2.rank must_=== 1

      val o3 = o2.nav.firstChild.get
      o3.rank must_== 2

      o3.nav.firstChild must_=== None

      val o4 = o3.nav.nextSibling.get
      o4.rank must_== 4

      o4.nav.firstChild must_=== None
      o4.nav.nextSibling must_=== None

      val o5 = o2.nav.nextSibling.get
      o5.rank must_=== 7

      o5.nav.firstChild must_=== None
      o5.nav.nextSibling must_=== None

      val c1 = o1.nav.findClose
      c1.get.rank must_=== 9
    }

//    "supports subtext operation" in {
//      val cursor: JsonCursor = JsonIO.loadWithIndex(
//        """{"a": 1, "b": 2}""")
//
//      println("--> a:" + cursor.nav)
//      println("--> b:" + cursor.nav.findClose)
//
//      cursor.subtext must_=== """{"a": 1, "b": 2}"""
//      cursor.nav.firstChild.get.subtext must_=== """ "a" """.trim
//      cursor.nav.firstChild.nextSibling.get.subtext must_=== """ "a" """.trim
//    }
  }
}
