package org.stuart.advent21

object Direction extends Enumeration {
  type Dir = Value
  val forward, down, up = Value
}

final case class Heading(direction: Direction.Dir, magnitude: Int)

final case class Location(depth: Int, horizontalPosition: Int, aim: Int)
