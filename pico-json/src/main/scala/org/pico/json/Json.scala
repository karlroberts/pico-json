package org.pico.json

sealed trait LazyJson {
  def fields: Stream[LazyJsonEntry] = Stream.empty

  def elements: Stream[LazyJson] = Stream.empty
}

case class LazyJsonObject(
    override val fields: Stream[LazyJsonEntry]) extends LazyJson {
  override def toString: String = {
    val sb = new StringBuilder()

    identity(fields.addString(sb, "{", ", ", "}"))

    sb.toString
  }
}

case class LazyJsonEntry(
    key: LazyJson,
    value: LazyJson) {
  override def toString: String = s"$key: $value"
}

case class LazyJsonArray(
    override val elements: Stream[LazyJson]) extends LazyJson {
  override def toString: String = {
    val sb = new StringBuilder()

    identity(elements.addString(sb, "[", ", ", "]"))

    sb.toString
  }
}

case class LazyJsonString(
    value: String) extends LazyJson {
  override def toString: String = {
    val sb = new StringBuilder()

    var i = 0

    sb.append('"')

    while (i < value.length) {
      if (value(i) == '\\') {
        sb.append("\\\\")
      } else if (value(i) == '"') {
        sb.append("\\\"")
      } else if (value(i) == '\t') {
        sb.append("\\t")
      } else if (value(i) == '\n') {
        sb.append("\\n")
      } else {
        sb.append(value(i))
      }
      i += 1
    }

    sb.append('"')

    sb.toString
  }
}

case class LazyJsonNumber(
    value: Double) extends LazyJson {
  override def toString: String = value.toString
}

case class LazyJsonBoolean(
    value: Boolean) extends LazyJson {
  override def toString: String = if (value) "true" else "false"
}

case object LazyJsonNull extends LazyJson

case object LazyJsonError extends LazyJson
