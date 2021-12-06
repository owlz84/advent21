package org.stuart.advent21

trait Sonar {
  def countConsecutiveIncreasing(trace: List[Int]): Int = {
    trace.init zip trace.drop(1) filter ({ case (prev: Int, curr: Int) =>
      curr > prev
    }) length
  }
  def sumFixedWindow(trace: List[Int]): List[Int] = {
    (trace.init, trace.drop(1), trace.drop(2)).zipped.toList.map({
      case (a: Int, b: Int, c: Int) => a + b + c
    })
  }
}
