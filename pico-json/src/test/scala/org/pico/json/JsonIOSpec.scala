package org.pico.json

import org.specs2.mutable.Specification

class JsonIOSpec extends Specification {
  "Can load json from string" in {
    val cursor = JsonIO.loadWithIndex(
      """
        |{ "object":
        |  { "number": 1
        |  , "true": true
        |  , "false": false
        |  , "null": null
        |  , "string": "text"
        |  , "array": [1, true, false, null, "text"]
        |  }
        |}
      """.stripMargin.trim)

    ok
  }
}
