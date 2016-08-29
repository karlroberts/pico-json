package org.pico.json

sealed trait JsonState

case object InJson extends JsonState
case object InString extends JsonState
case object InEscape extends JsonState
