package org.pico.json

case class BitVector(array: Array[Long]) extends AnyVal {
  def update(position: Int, value: Boolean): Unit = {
    val i = position >>> 6
    val j = position & 0x3f

    if (value) {
      array(i) |= (1L << j)
    } else {
      array(i) &= ~(1L << j)
    }

    ()
  }

  def apply(position: Int): Boolean = {
    val i = position >>> 6
    val j = position & 0x3f

    (array(i) & (1 << j)) != 0
  }

  def length: Int = array.length * 64
}

object BitVector {
  def ofLength(bitSize: Int): BitVector = {
    BitVector(new Array[Long]((bitSize + 63) / 64))
  }

  def fromBalancedParens(text: String): BitVector = {
    val bv = BitVector.ofLength(text.length)

    var i = 0

    text.foreach {
      case '(' =>
        bv(i) = true
        i += 1
      case ')' =>
        bv(i) = false
        i += 1
      case _ =>
    }

    bv
  }

  def fromBitString(text: String): BitVector = {
    val bv = BitVector.ofLength(text.length)

    var i = 0

    text.foreach {
      case '1' =>
        bv(i) = true
        i += 1
      case '0' =>
        bv(i) = false
        i += 1
      case _ =>
    }

    bv
  }
}
