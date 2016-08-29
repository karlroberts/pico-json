package org.pico.json

sealed trait LazyJson

case class LazyJsonObject(fields: Stream[(String, LazyJson)]) extends LazyJson

case class LazyJsonArray(elements: Stream[LazyJson]) extends LazyJson

case class LazyJsonString(value: String) extends LazyJson

case class LazyJsonNumber(value: Double) extends LazyJson

case class LazyJsonBoolean(value: Boolean) extends LazyJson

case object LazyJsonNull extends LazyJson
