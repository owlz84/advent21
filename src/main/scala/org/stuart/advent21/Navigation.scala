package org.stuart.advent21

import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.{Queue, HashSet, Stack}

object Navigation {

  class SyntaxChecker(codeLines: List[String]) extends LazyLogging {
    val openingChars = List('(', '[', '{', '<')
    val closingChars = List(')', ']', '}', '>')
    val corruptCharScores = List(3, 57, 1197, 25137)
    val corruptCharScoreMap = closingChars.zip(corruptCharScores).toMap
    val autoCorrectScoreMap = closingChars.zipWithIndex
      .map({ case (c: Char, s: Int) => (c, s + 1) })
      .toMap

    case class Line(chrs: Array[Char]) {
      val firstIllegalChar: Option[Char] = {
        var returnVal: Option[Char] = None
        val charPairMap = closingChars.zip(openingChars).toMap
        var charStack = new Stack[Char]
        var working = true
        chrs
          .takeWhile(_ => working)
          .foreach(c => {
            if (openingChars contains c) charStack.push(c)
            if (closingChars contains c) {
              if (charPairMap(c) != charStack.pop) {
                working = false
                returnVal = Some(c)
              }
            }
          })
        returnVal
      }
      val autoComplete: Option[String] = {
        var returnVal: Option[String] = None
        val charPairMap = openingChars.zip(closingChars).toMap
        var charStack = new Stack[Char]

        if (firstIllegalChar.isEmpty) {
          chrs
            .foreach(c => {
              if (openingChars contains c) charStack.push(c)
              if (closingChars contains c) charStack.pop()
            })
          returnVal = Some(charStack.toList.map(charPairMap(_)).mkString)
        }
        returnVal
      }
    }

    def getCorruptLinesScore: Option[Long] = {
      val corruptCodeLines = codeLines
        .map(line => Line(line.toCharArray).firstIllegalChar)
        .filterNot(_.isEmpty)

      if (corruptCodeLines.isEmpty) return None

      val score = corruptCodeLines
        .map(_.get)
        .map(c => corruptCharScoreMap.getOrElse(c, 0).toLong)
        .sum

      return Some(score)
    }

    def getAutoCompleteScore: Option[Long] = {
      val incompleteCodeLines = codeLines
        .map(line => Line(line.toCharArray).autoComplete)
        .filterNot(_.isEmpty)
        .map(_.get.toCharArray)

      if (incompleteCodeLines.isEmpty) return None

      val score = incompleteCodeLines
        .map(cArray => {
          cArray
            .map(c => autoCorrectScoreMap(c).toLong)
            .reduceLeft(_ * 5 + _)
        })
        .sorted
        .apply((incompleteCodeLines.length - 1) / 2)

      Some(score)
    }
  }

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

  object Direction extends Enumeration {
    type Dir = Value
    val forward, down, up = Value
  }

  final case class Heading(direction: Direction.Dir, magnitude: Int)

  final case class Location(depth: Int, horizontalPosition: Int, aim: Int)

}
