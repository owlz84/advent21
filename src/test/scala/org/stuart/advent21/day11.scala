package org.stuart.advent21

import org.scalatest._
import com.typesafe.scalalogging.LazyLogging
import org.stuart.advent21.Wildlife.Octomesh

class Day11 extends FunSuite with Matchers with LazyLogging {

  test(
    "The octopus tracker should correctly predict the number of flashes in 100 steps"
  ) {
    val data = Day11.exampleInput
      .split("\n")
      .map(line => line.map(c => Integer.parseInt(c.toString)).toList)
      .toList
    val mesh = new Octomesh(data, 100)
    val result = mesh.sum
    result shouldBe Day11.exampleExpectedResultPart1
  }
  test("(generate submission for part 1)") {
    val data = Day11.input
      .split("\n")
      .map(line => line.map(c => Integer.parseInt(c.toString)).toList)
      .toList
    val mesh = new Octomesh(data, 100)
    val result = mesh.sum
    logger.info(s"result: $result")
  }
  test(
    "The octopus tracker should correctly predict when a synchronised flash occurs"
  ) {
    val data = Day11.exampleInput
      .split("\n")
      .map(line => line.map(c => Integer.parseInt(c.toString)).toList)
      .toList
    val mesh = new Octomesh(data, 200)
    val result = mesh.length
    result shouldBe Day11.exampleExpectedResultPart2
  }
  test(
    "(generate submission for part 2)"
  ) {
    val data = Day11.input
      .split("\n")
      .map(line => line.map(c => Integer.parseInt(c.toString)).toList)
      .toList
    val mesh = new Octomesh(data, 500)
    val result = mesh.length
    logger.info(s"result: $result")
  }

}

object Day11 {

  val exampleExpectedResultPart1 = 1656
  val exampleExpectedResultPart2 = 195

  val exampleInput =
    """5483143223
    |2745854711
    |5264556173
    |6141336146
    |6357385478
    |4167524645
    |2176841721
    |6882881134
    |4846848554
    |5283751526""".stripMargin

  val input =
    """6788383436
    |5526827441
    |4582435866
    |5152547273
    |3746433621
    |2465145365
    |6324887128
    |8537558745
    |4718427562
    |2283324746""".stripMargin
}
