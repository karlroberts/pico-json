package org.pico.json

import org.pico.json.syntax.balancedParens._
import org.pico.json.syntax.jsonCursor._
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

      o1.nav.ns must_=== None

      val o2 = o1.nav.fc.get
      o2.rank must_=== 1

      val o3 = o2.nav.fc.get
      o3.rank must_== 2

      o3.nav.fc must_=== None

      val o4 = o3.nav.ns.get
      o4.rank must_== 4

      o4.nav.fc must_=== None
      o4.nav.ns must_=== None

      val o5 = o2.nav.ns.get
      o5.rank must_=== 7

      o5.nav.fc must_=== None
      o5.nav.ns must_=== None

      val c1 = o1.nav.findClose
      c1.get.rank must_=== 9
    }

    "supports subtext operation" in {
      val cursor: JsonCursor = JsonIO.loadWithIndex(
        """{"a": 1, "b": [1, true, false, {}]}""")

      cursor.subtext must_=== """{"a": 1, "b": [1, true, false, {}]}"""
      cursor.nav.fc.get.subtext must_=== "\"a\""
      cursor.nav.fc.ns.get.subtext must_=== " 1"
      cursor.nav.fc.ns.ns.get.subtext must_=== " \"b\""
      cursor.nav.fc.ns.ns.ns.get.subtext must_=== " [1, true, false, {}]"
      cursor.nav.fc.ns.ns.ns.fc.get.subtext must_=== "[1, true, false, {}]"
      cursor.nav.fc.ns.ns.ns.fc.fc.get.subtext must_=== "1"
      cursor.nav.fc.ns.ns.ns.fc.fc.ns.get.subtext must_=== " true"
      cursor.nav.fc.ns.ns.ns.fc.fc.ns.ns.get.subtext must_=== " false"
      cursor.nav.fc.ns.ns.ns.fc.fc.ns.ns.ns.get.subtext must_=== " {}"
    }

    "supports lazyJson operation" in {
      val cursor: JsonCursor = JsonIO.loadWithIndex(
        """{"a": 1, "\"b\"": [{"x": 1}, 1, true, false]}""")

      val json = cursor.nav.lazyJson

      json.toString must_===
        """
          |{"a": 1.0, ?}
        """.stripMargin.trim

      json.fields.foreach { kv =>
      }

      json.toString must_==
        """
          |{"a": 1.0, "\"b\"": [{"x": 1.0, ?}, ?]}
        """.stripMargin.trim

      json.fields.foreach { kv =>
        kv.value.elements.foreach { e =>
        }
      }

      json.toString must_==
          """
            |{"a": 1.0, "\"b\"": [{"x": 1.0, ?}, 1.0, true, false]}
          """.stripMargin.trim

      json.fields.foreach { kv =>
        kv.value.elements.foreach { e =>
          e.fields.foreach { kv2 =>
          }
        }
      }

      json.toString must_==
          """
            |{"a": 1.0, "\"b\"": [{"x": 1.0}, 1.0, true, false]}
          """.stripMargin.trim

      ok
    }
  }
}
