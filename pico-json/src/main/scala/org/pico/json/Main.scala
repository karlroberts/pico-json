package org.pico.json

import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val file = new File("data.json")
    val text = JsonIO.loadJsonAsArray(file)

  }
}
