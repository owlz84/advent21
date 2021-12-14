package org.stuart.advent21

import com.typesafe.scalalogging.LazyLogging

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
      def adjacents: List[Int] = {
        val offsets = Array(-1, 1)
        def offsetToPointList(offset: Array[(Int, Int)]): List[Int] = offset
          .filter(_._1 >= 0)
          .filter(_._2 >= 0)
          .filter(_._1 < mapSize._1)
          .filter(_._2 < mapSize._2)
          .map(p => surface(p._2)(p._1))
          .toList
        val xAdjacents = offsetToPointList(offsets.map((d: Int) => (x + d, y)))
        val yAdjacents = offsetToPointList(offsets.map((d: Int) => (x, y + d)))
        xAdjacents ++ yAdjacents
      }
      val isLowPoint = adjacents.filter(_ <= height).length == 0
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
  }

}
