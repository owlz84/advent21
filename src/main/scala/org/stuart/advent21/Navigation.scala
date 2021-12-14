package org.stuart.advent21

import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.{Queue, HashSet}

object Navigation {
  object Direction extends Enumeration {
    type Dir = Value
    val forward, down, up = Value
  }

  final case class Heading(direction: Direction.Dir, magnitude: Int)

  final case class Location(depth: Int, horizontalPosition: Int, aim: Int)

  class CaveHeightMap(surface: Array[Array[Int]]) extends LazyLogging {

    val mapSize = (surface.head.length, surface.length)

    case class Point(x: Int, y: Int, height: Int) {
      val riskLevel = 1 + height
      val adjacentOffsets = {
        val offsets = Array(-1, 1)
        val xOffsets = offsets.map((d: Int) => (x + d, y))
        val yOffsets = offsets.map((d: Int) => (x, y + d))
        xOffsets ++ yOffsets
      }

      val adjacentCoords: Array[(Int, Int)] = adjacentOffsets
        .filter(_._1 >= 0)
        .filter(_._2 >= 0)
        .filter(_._1 < mapSize._1)
        .filter(_._2 < mapSize._2)

      val adjacentHeights = adjacentCoords
        .map(p => surface(p._2)(p._1))
        .toList

      def getAdjacents: List[Point] = {
        adjacentCoords
          .map({ case (aX: Int, aY: Int) => Point(aX, aY, surface(aY)(aX)) })
          .toList
      }
      val isLowPoint = adjacentHeights.filter(_ <= height).length == 0
      val isWaterShed = height == 9
      def getBasin: List[Point] = {
        var adjacentPoints = new Queue[Point]
        var seenPoints = new HashSet[Point]
        adjacentPoints.enqueue(this)
        while (adjacentPoints.length > 0) {
          val currentPoint = adjacentPoints.dequeue()
          seenPoints += currentPoint
          adjacentPoints ++= currentPoint.getAdjacents
            .filterNot(_.isWaterShed)
            .filterNot(seenPoints contains _)
        }
        seenPoints.toList
      }
    }
    val allPoints = surface.zipWithIndex
      .map({ case (row: Array[Int], i: Int) =>
        row.zipWithIndex
          .map({ case (point: Int, j: Int) => new Point(j, i, point) })
      })
      .flatten

    val lowPoints = allPoints.filter(_.isLowPoint)
    val totalRiskScore = {
      lowPoints.map(p => p.riskLevel).sum
    }
    def getBasinSizes: Int = {
      lowPoints
        .map(_.getBasin)
        .map(_.length)
        .sorted
        .reverse
        .take(3)
        .reduceLeft(_ * _)
    }
  }

}
