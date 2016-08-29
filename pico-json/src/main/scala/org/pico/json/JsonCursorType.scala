package org.pico.json

sealed trait JsonCursorType

case object JsonCursorObject extends JsonCursorType
case object JsonCursorArray extends JsonCursorType
case object JsonCursorString extends JsonCursorType
case object JsonCursorNumber extends JsonCursorType
case object JsonCursorBoolean extends JsonCursorType
case object JsonCursorNull extends JsonCursorType
case object JsonCursorError extends JsonCursorType
