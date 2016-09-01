package org.pico.json.syntax

import org.pico.json._
import org.pico.json.syntax.balancedParens._

package object jsonCursor {
  implicit class StringOps(val self: String) extends AnyVal {
    def parseAsString: String = {
      val a = self.trim
      val b = if (a.startsWith("\"")) a.tail else a
      val c = if (b.endsWith("\"")) b.init else b

      StringContext treatEscapes c
    }
  }

  implicit class OptionJsonCursorOps(val self: Option[JsonCursor]) extends AnyVal {
    def lazyJsonElements: Stream[LazyJson] = {
      if (self.isEmpty) {
        Stream.empty
      } else {
        self.lazyJsonImpl #:: self.ns.lazyJsonElements
      }
    }

    def lazyJsonEntries: Stream[LazyJsonEntry] = {
      def go(elements: Stream[LazyJson]): Stream[LazyJsonEntry] = {
        elements match {
          case k #:: v #:: remainder  => LazyJsonEntry(k, v) #:: go(remainder)
          case _                      => Stream.empty
        }
      }

      go(lazyJsonElements)
    }

    def subtext: String = {
      self match {
        case Some(cursor) => cursor.subtext
        case None => ""
      }
    }

    def lazyJson: LazyJson = {
      jsonType match {
        case JsonCursorObject => LazyJsonObject(self.fc.lazyJsonEntries)
        case JsonCursorArray => LazyJsonArray(self.fc.lazyJsonElements)
        case JsonCursorString => LazyJsonString(self.subtext.parseAsString)
        case JsonCursorNumber => LazyJsonNumber(self.subtext.trim.toDouble)
        case JsonCursorBoolean => LazyJsonBoolean(self.subtext.trim.charAt(0) == 't')
        case JsonCursorNull => LazyJsonNull
        case JsonCursorError => LazyJsonError
      }
    }

    def lazyJsonImpl: LazyJson = {
      jsonType match {
        case JsonCursorObject => LazyJsonObject(self.fc.fc.lazyJsonEntries)
        case JsonCursorArray => LazyJsonArray(self.fc.fc.lazyJsonElements)
        case JsonCursorString => LazyJsonString(self.subtext.parseAsString)
        case JsonCursorNumber => LazyJsonNumber(self.subtext.trim.toDouble)
        case JsonCursorBoolean => LazyJsonBoolean(self.subtext.trim.charAt(0) == 't')
        case JsonCursorNull => LazyJsonNull
        case JsonCursorError => LazyJsonError
      }
    }

    def jsonType: JsonCursorType = {
      self match {
        case Some(cursor)  =>
          cursor.subtext.dropWhile(_ == ' ').headOption match {
            case Some(c) =>
              if ("+-0123456789".contains(c)) {
                JsonCursorNumber
              } else if (c == '"') {
                JsonCursorString
              } else if (c == 't') {
                JsonCursorBoolean
              } else if (c == 'f') {
                JsonCursorBoolean
              } else if (c == 'n') {
                JsonCursorNull
              } else if (c == '[') {
                JsonCursorArray
              } else if (c == '{') {
                JsonCursorObject
              } else {
                JsonCursorError
              }
            case None => JsonCursorError
          }
        case None => JsonCursorError
      }
    }
  }
}
