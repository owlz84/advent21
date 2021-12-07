package org.stuart.advent21

class Crabs(locations: List[Int], costFactor: Int = 0) {
  val candidates = (locations.min to locations.max)
  def fuelCost(first_element: Int, step: Int, seq_length: Int): Int = 
    seq_length match {
      case 0 => 0
      case _ => first_element + fuelCost(first_element + step, step, seq_length - 1)
  }

  def calcFuelConsumption(candidateLocation: Int) = locations
    .map(_ - candidateLocation)
    .map(Math.abs)
    .map(distance => fuelCost(1, costFactor, distance))
  .sum
  def bestLocation = candidates
    .map(candidate => (candidate, calcFuelConsumption(candidate)))
  .sortBy(_._2)
  .head._2
}
