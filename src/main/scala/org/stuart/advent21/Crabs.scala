package org.stuart.advent21

class Crabs(locations: List[Int], costFactor: Int = 0) {

  // def fuelCost(first_element: Int, step: Int, seq_length: Int): Int =
  //   seq_length match {
  //     case 0 => 0
  //     case _ =>
  //       first_element + fuelCost(first_element + step, step, seq_length - 1)
  //   }

  def fuelCost(distance: Int) = distance * (distance + 1) / 2

  def calcFuelConsumption(candidateLocation: Int) = locations
    .map(_ - candidateLocation)
    .map(Math.abs)
    .map(distance =>
      costFactor match {
        case 0 => distance
        case _ => fuelCost(distance)
      }
    )
    .sum
  def bestLocation =
    (locations.min to locations.max)
      .map(candidate => (candidate, calcFuelConsumption(candidate)))
      .sortBy(_._2)
      .head
      ._2
}
