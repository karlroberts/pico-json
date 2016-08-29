package org.pico.json

import org.pico.json.syntax.long._
import org.specs2.mutable.Specification

class BitVectorSpec extends Specification {
  "A BitVector" should {
    "support setting bit" in {
      val bitVector = BitVector(new Array[Long](1))

      (0 until 64).foreach { i =>
        bitVector(i) must_=== false
      }

      ok
    }

    "when setting all bits give fully set long" in {
      val bitVector = BitVector(new Array[Long](1))

      (0 until 64).foreach { i =>
        bitVector(i) = true
      }

      bitVector.array(0) must_=== 0xFFFFFFFFFFFFFFFFL
    }

    "when setting one bit should produce long with one bit count" in {
      (0 until 64).foreach { i =>
        val bitVector = BitVector(new Array[Long](1))

        bitVector(i) = true

        bitVector.array(0).bitCount must_=== 1
      }

      ok
    }

    "when setting consecutive bits process longs in multiples of 2" in {
      (0 until 63).foreach { i =>
        val bitVector0 = BitVector(new Array[Long](1))
        val bitVector1 = BitVector(new Array[Long](1))

        bitVector0(i) = true
        bitVector1(i + 1) = true

        bitVector1.array(0) must_=== bitVector0.array(0) * 2
      }

      ok
    }

    "when setting bits 64 bits apart should result in same longs" in {
      (0 until 64).foreach { i =>
        val bitVector = BitVector(new Array[Long](2))

        bitVector(i) = true
        bitVector(i + 64) = true

        bitVector.array(0) must_=== bitVector.array(1)
      }

      ok
    }

    "when constructing from balanced parens string" in {
      val bv: BitVector = BitVector.fromBalancedParens("((()())())")

      bv(0) must_=== true
      bv(1) must_=== true
      bv(2) must_=== true
      bv(3) must_=== false
      bv(4) must_=== true
      bv(5) must_=== false
      bv(6) must_=== false
      bv(7) must_=== true
      bv(8) must_=== false
      bv(9) must_=== false
    }
  }
}
