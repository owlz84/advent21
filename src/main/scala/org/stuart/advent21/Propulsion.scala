package org.stuart.advent21

import org.stuart.advent21.Navigation.{Location, Heading, Direction}
import com.typesafe.scalalogging.LazyLogging

trait Propulsion extends LazyLogging {
  var location = Location(0, 0, 0)
  def move(vector: Heading) = {
    val aimChange = vector.direction match {
      case Direction.down => vector.magnitude
      case Direction.up   => -1 * vector.magnitude
      case _              => 0
    }
    val positionChange = vector.direction match {
      case Direction.forward => vector.magnitude
      case _                 => 0
    }
    val newAim = location.aim + aimChange
    val depthChange = newAim * positionChange
    location = Location(
      location.depth + depthChange,
      location.horizontalPosition + positionChange,
      newAim
    )
    logger.debug(location.toString())
  }
}
