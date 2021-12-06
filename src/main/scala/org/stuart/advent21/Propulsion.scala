package org.stuart.advent21

import org.stuart.advent21.navigation.{Location, Heading, Direction}

trait Propulsion {
  var location = Location(0, 0)
  def move(vector: Heading) = {
    val depthChange = vector.direction match {
      case Direction.down => vector.magnitude
      case Direction.up => -1 * vector.magnitude
      case _ => 0
    }
    val positionChange = vector.direction match {
      case Direction.forward => vector.magnitude
      case _ => 0
    }
    location = Location(location.depth + depthChange, location.position + positionChange)
  }
}
