package org.pico.json

import scala.annotation.tailrec

case class BitVector(array: Array[Long]) extends AnyVal {
  override def toString: String = {
    val result = (0 until length).map { i =>
      if (this(i)) "1" else "0"
    }.mkString

    result
  }

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

  def select(n: Int): Option[Int] = {
    @tailrec
    def go(i: Int, position: Int): Int = {
      if (position < length) {
        if (i == 0) {
          position
        } else if (this(position)) {
          go(i - 1, position + 1)
        } else {
          go(i, position + 1)
        }
      } else {
        -1
      }
    }

    val result = go(n, 0)

    if (result != -1) {
      Some(result)
    } else {
      None
    }
  }

  def apply(position: Int): Boolean = {
    val i = position >>> 6
    val j = position & 0x3f

    (array(i) & (1L << j)) != 0
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
