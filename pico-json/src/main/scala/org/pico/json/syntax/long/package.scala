package org.pico.json.syntax

package object long {
  implicit class LongOps(val self: Long) extends AnyVal {
    def bitCount: Int = java.lang.Long.bitCount(self)
  }
}
