package org.pico.json

trait JsonEventHandler {
  def openBracket(position: Int): Unit

  def closeBracket(position: Int): Unit

  def openBrace(position: Int): Unit

  def closeBrace(position: Int): Unit

  def comma(position: Int): Unit

  def colon(position: Int): Unit
}
