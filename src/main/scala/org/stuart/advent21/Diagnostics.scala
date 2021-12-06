package org.stuart.advent21

import com.typesafe.scalalogging.LazyLogging

trait Diagnostics extends LazyLogging {

  def getGammaString(report: List[String]) = {
    report.transpose.map(
      _.groupBy(identity).toList.sortBy(_._2.length).tail.map(_._1).head
    ).mkString("")
  }

  def getEpsilonString(gammaString: String) = {
    gammaString.map(c => if (c == "0".charAt(0)) "1" else "0").mkString("")
  }

  def getPowerConsumption(report: List[String]): Int = {
    val gammaString = getGammaString(report)
    val epsilonString = getEpsilonString(gammaString)

    val gamma = Integer.parseInt(gammaString, 2)
    val epsilon = Integer.parseInt(epsilonString, 2)

    logger.debug(s"Gamma: $gamma, Epsilon: $epsilon")
    
    gamma * epsilon 
  }
}
