package org.stuart.advent21

import org.scalatest._
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ArrayBuffer

class day06
    extends FunSuite
    with Matchers
    with BeforeAndAfterEach
    with LazyLogging {

  test("My simulation should correctly model the Lanternfish population when they live for 8 days") {
    val daysToRun = 80
    val population = new Lanternfish
    population.addFish(day06.testData)
    val result = (1 to daysToRun).map(i => population.next()).max
    result shouldBe day06.testExpectedPart1
  }

  test("(submission for part 1)") {
    val daysToRun = 80
    val population = new Lanternfish
    population.addFish(day06.input)
    val result = (1 to daysToRun).map(i => population.next()).max
    logger.info(s"result: $result")
  }

  test("My simulation should correctly model the Lanternfish population when they live for 256 days") {
    val daysToRun = 256
    val population = new Lanternfish
    population.addFish(day06.testData)
    val result = (1 to daysToRun).map(i => population.next()).max
    result shouldBe day06.testExpectedPart2
  }

  test("(submission for part 2)") {
    val daysToRun = 256
    val population = new Lanternfish
    population.addFish(day06.input)
    val result = (1 to daysToRun).map(i => population.next()).max
    logger.info(s"result: $result")
  }

}

object day06 {
  val testData = List(3, 4, 3, 1, 2)
  val testExpectedPart1 = 5934L
  val testExpectedPart2 = 26984457539L

  val input = List(5, 1, 1, 3, 1, 1, 5, 1, 2, 1, 5, 2, 5, 1, 1, 1, 4, 1, 1, 5,
    1, 1, 4, 1, 1, 1, 3, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 1, 1, 1, 1, 1,
    4, 1, 1, 1, 1, 1, 5, 1, 1, 1, 4, 1, 1, 1, 1, 1, 3, 1, 1, 4, 1, 4, 1, 1, 2,
    3, 1, 1, 1, 1, 4, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 2, 1, 1, 1,
    1, 1, 1, 1, 4, 4, 1, 4, 2, 1, 1, 1, 1, 1, 4, 3, 1, 1, 1, 1, 2, 1, 1, 1, 2,
    1, 1, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1,
    1, 1, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 3, 1, 2, 1, 1, 4, 1, 1, 5, 3, 1, 1,
    1, 2, 4, 1, 1, 2, 4, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 4,
    3, 1, 2, 1, 2, 1, 5, 1, 2, 1, 1, 5, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1,
    1, 1, 1, 3, 1, 1, 5, 1, 1, 1, 1, 5, 1, 4, 1, 1, 1, 4, 1, 3, 4, 1, 4, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 3, 5, 1, 3, 1, 1, 1, 1, 4, 1, 5, 3, 1, 1, 1, 1, 1, 5,
    1, 1, 1, 2, 2)
}
