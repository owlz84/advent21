package org.stuart.advent21

import scala.collection.mutable
import com.typesafe.scalalogging.LazyLogging

object Wildlife {
  class Octomesh(startingEnergy: List[List[Int]], steps: Int)
      extends Iterator[Int]
      with LazyLogging {
    var currentStep = 0
    val meshSize = List(startingEnergy.head.length, startingEnergy.length)
    var currentEnergy =
      mutable.ListBuffer.tabulate[Int](meshSize(0), meshSize(1))((i, j) => 0)

    case class Octopus(x: Int, y: Int) {
      val adjacentOffsets = {
        val rawOffsets = List(-1, 0, 1)
        val offsets = for { oX <- rawOffsets; oY <- rawOffsets } yield (oX, oY)
        offsets.filterNot(_ == (0, 0))
      }
      val adjacentCoords: List[(Int, Int)] = adjacentOffsets
        .map({ case (oX: Int, oY: Int) => (x + oX, y + oY) })
        .filter(_._1 >= 0)
        .filter(_._2 >= 0)
        .filter(_._1 < meshSize(0))
        .filter(_._2 < meshSize(1))
      def getEnergy: Int = currentEnergy(y)(x)
      def setEnergy(energy: Int) = currentEnergy(y)(x) = energy
      def incEnergy = setEnergy(getEnergy + 1)
      var flashed = false
      def getAdjacents: List[Octopus] = {
        adjacentCoords
          .map({ case (aX: Int, aY: Int) => allOctopuses(aY)(aX) })
          .toList
      }
    }

    var allOctopuses = startingEnergy.zipWithIndex
      .map({ case (row: List[Int], i: Int) =>
        row.zipWithIndex
          .map({
            case (energy: Int, j: Int) => {
              val o = new Octopus(j, i)
              o.setEnergy(energy)
              o
            }
          })
      })

    def hasNext = {
    currentStep < steps & currentEnergy.flatten.sum > 0
  }
    def next = {
      var flashCount = 0
      currentStep += 1
      allOctopuses.flatten.foreach(_.incEnergy)
      var octopusQueue =
        mutable.Queue(allOctopuses.flatten.filter(_.getEnergy > 9): _*)
      while (octopusQueue.length > 0) {
        val o = octopusQueue.dequeue
        flashCount += 1
        o.flashed = true
        val adjacents = o.getAdjacents
        adjacents.foreach(_.incEnergy)
        octopusQueue.enqueue(
          adjacents
            .filterNot(_.flashed)
            .filter(_.getEnergy > 9)
            .filterNot(octopusQueue contains _): _*
        )
      }
      allOctopuses.flatten.foreach(o =>
        if (o.flashed) { o.setEnergy(0); o.flashed = false }
      )
      // logger.debug(s"step: $currentStep, flashCount: $flashCount")
      flashCount
    }
  }

  class Lanternfish(maxAge: Int = 8, resetAge: Int = 6) extends Iterator[Long] {
    var ageFreq: mutable.Map[Int, Long] = mutable.Map()
    def addFish(ages: List[Int]) = {
      ageFreq = mutable.Map((-1 to maxAge).map(id => (id, 0L)): _*)
      ages.map(ageFreq(_) += 1)
    }
    def hasNext: Boolean = true
    def next: Long = {
      ageFreq = ageFreq.map({ case (age: Int, pop: Long) => (age - 1, pop) })
      val overflow = ageFreq(-1)
      ageFreq.remove(maxAge)
      ageFreq.remove(-1)
      ageFreq(maxAge) = overflow
      ageFreq(resetAge) += overflow
      ageFreq.values.sum
    }
  }
  class Crabs(locations: List[Int], costFactor: Int = 0) {

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
}
