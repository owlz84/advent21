package org.stuart.advent21

import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.{Map => MutableMap}

class Lanternfish(maxAge: Int = 8, resetAge: Int = 6) extends Iterator[Long] with LazyLogging {
  var ageFreq: MutableMap[Int, Long] = MutableMap()
  def addFish(ages: List[Int]) = {
    ageFreq = MutableMap((-1 to maxAge).map(id => (id, 0L)): _*)
    ages.map(ageFreq(_) += 1)
  }
  def hasNext: Boolean = true
  def next: Long = {
    ageFreq = ageFreq.map({case (age: Int, pop: Long) => (age - 1, pop)})
    val overflow = ageFreq(-1)
    ageFreq.remove(maxAge)
    ageFreq.remove(-1)
    ageFreq(maxAge) = overflow
    ageFreq(resetAge) += overflow
    ageFreq.values.sum
  }
}
