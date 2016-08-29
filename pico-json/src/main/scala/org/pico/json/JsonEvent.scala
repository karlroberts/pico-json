package org.pico.json

object JsonEvent {
  val DoubleQuote = '"'.toByte
  val Comma = ','.toByte
  val Colon = ':'.toByte
  val OpenBrace = '{'.toByte
  val CloseBrace = '}'.toByte
  val OpenBracket = '['.toByte
  val CloseBracket = ']'.toByte
  val BackSlash = '\\'.toByte

  def play(text: Array[Byte])(handler: JsonEventHandler): Unit = {
    var i: Int = 0
    var s: JsonState = InJson

    while (i < text.length) {
      s match {
        case InJson =>
          text(i) match {
            case DoubleQuote => s = InString
            case Comma => handler.comma(i)
            case Colon => handler.colon(i)
            case OpenBrace => handler.openBrace(i)
            case CloseBrace => handler.closeBrace(i)
            case OpenBracket => handler.openBracket(i)
            case CloseBracket => handler.closeBracket(i)
            case _ =>
          }
        case InString =>
          text(i) match {
            case DoubleQuote => s = InJson
            case BackSlash => s = InEscape
            case _ =>
          }
        case InEscape =>
          s = InString
      }

      i += 1
    }
  }
}
