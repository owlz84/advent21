package org.stuart.advent21

import com.typesafe.scalalogging.LazyLogging
import scala.collection.immutable.ListMap

trait Diagnostics extends LazyLogging {

  def getBitFrequency(bitString: List[Char]): Map[Char, Int] = {
    bitString
      .groupBy(identity)
      .toMap
      .mapValues(_.length)
  }

  def getMostCommonBit(bitString: List[Char]): Char = {
    val bitFrequencies = getBitFrequency(bitString)
    val mostCommon = ListMap(bitFrequencies.toSeq.sortBy(_._2): _*).head
    mostCommon._1
  }

  def getGammaString(report: List[String]): String = {
    report.transpose.map(getMostCommonBit).mkString("")
  }

  def getEpsilonString(gammaString: String): String = {
    gammaString
      .map(c => if (c == '0') "1" else "0")
      .mkString("")
  }

  def getPowerConsumption(report: List[String]): Int = {
    val gammaString = getGammaString(report)
    val epsilonString = getEpsilonString(gammaString)

    val gamma = Integer.parseInt(gammaString, 2)
    val epsilon = Integer.parseInt(epsilonString, 2)

    logger.debug(s"Gamma: $gamma, Epsilon: $epsilon")

    gamma * epsilon
  }

  def getRating(report: List[String], highBit: Char, lowBit: Char): String = {
    var filteredReport = report
    for (i <- 0 to report.head.length) {
      val bits = filteredReport.transpose.apply(i)
      val bitCounts = getBitFrequency(bits)
      val filterBit = if (bitCounts('0') > bitCounts('1')) lowBit else highBit
      val indices = bits.zipWithIndex
        .collect({ case (bit, idx) if bit == filterBit => idx })
      if (indices.length == 1) return indices.map(filteredReport).head
      filteredReport = indices.map(filteredReport)
    }
    filteredReport.head
  }

  def getLifeSupportRating(report: List[String]): Int = {
    val oxygenRating = Integer.parseInt(getRating(report, '1', '0'), 2)
    val CO2Rating = Integer.parseInt(getRating(report, '0', '1'), 2)
    logger.debug(s"Oxygen rating: $oxygenRating, CO2 rating: $CO2Rating")
    oxygenRating * CO2Rating
  }

}
