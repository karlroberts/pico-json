package org.pico.json

import java.io._

object JsonIO {

  def loadJsonAsArray(file: File): Array[Byte] = {
    val is = new FileInputStream(file)

    try {
      loadJsonAsArray(is, file.length().toInt)
    } finally {
      is.close()
    }
  }

  def loadJsonAsArray(is: InputStream, length: Int): Array[Byte] = {
    val array = new Array[Byte](length)
    is.read(array)
    array
  }

  def loadLongArray(is: InputStream, length: Int): Array[Long] = {
    val in = new DataInputStream(is)
    val array = new Array[Long](length / 8 + 1)

    var i = 0

    try {
      while (true) {
        array(i) = in.readLong()
        i += 1
      }
    } catch {
      case e: EOFException =>
    } finally {
      in.close()
      is.close()
    }

    array
  }

  def loadWithIndex(file: File): JsonCursor = loadWithIndex(loadJsonAsArray(file))

  def loadWithIndex(text: String): JsonCursor = loadWithIndex(text.getBytes("UTF-8"))

  def loadWithIndex(text: Array[Byte]): JsonCursor = {
    val ibs = BitVector.ofLength(text.length)
    val bps = BitVector.ofLength(text.length * 2)

    JsonEvent.play(text)(new JsonEventHandler {
      var bpi: Int = 0

      override def openBracket(position: Int): Unit = {
        ibs(position) = true

        bps(bpi) = true
        bps(bpi + 1) = true
        bpi += 2
      }

      override def closeBracket(position: Int): Unit = {
        ibs(position) = true

        bps(bpi) = false
        bps(bpi + 1) = false
        bpi += 2
      }

      override def openBrace(position: Int): Unit = {
        ibs(position) = true

        bps(bpi) = true
        bps(bpi + 1) = true
        bpi += 2
      }

      override def closeBrace(position: Int): Unit = {
        ibs(position) = true

        bps(bpi) = false
        bps(bpi + 1) = false
        bpi += 2
      }

      override def comma(position: Int): Unit = {
        ibs(position) = true

        bps(bpi) = false
        bps(bpi + 1) = true
        bpi += 2
      }

      override def colon(position: Int): Unit = {
        ibs(position) = true

        bps(bpi) = false
        bps(bpi + 1) = false
        bpi += 2
      }
    })

    JsonCursor(new String(text, "UTF-8"), ibs, bps, 0)
  }
}
