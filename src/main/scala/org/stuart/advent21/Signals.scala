package org.stuart.advent21

object Signals {

  case class Pattern(chars: Set[Char])
  case class Signal(patterns: List[Pattern])
  case class Setting(wiring: List[Char])

  class DataPreProcessor(data: String) {
    val signalStrings = data.split("\n").map(line => line.split(" \\| "))
    def toSignal(signalString: Array[String]): List[Signal] = 
      signalString
      .map(line => line.split(" ").map(s => Pattern(s.toSet)).toList)
      .map(Signal).toList
    val inputSignals = toSignal(signalStrings.map(io => io(0)))
    val outputSignals = toSignal(signalStrings.map(io => io(1)))
  }

  class SignalProcessor {

    val uniqueNumLengths = List(2, 3, 4, 7)

    def uniqueNums(signal: Signal) = signal.patterns
      .filter(pattern => uniqueNumLengths.contains(pattern.chars.size))
      .size

    val inputWiringPermutations =
      ('a' to 'g').permutations.map(_.toList).map(Setting(_))

    val segmentSettingMap = Map(
      "0" -> List(1, 2, 3, 5, 6, 7),
      "1" -> List(3, 6),
      "2" -> List(1, 3, 4, 5, 7),
      "3" -> List(1, 3, 4, 6, 7),
      "4" -> List(2, 3, 4, 6),
      "5" -> List(1, 2, 4, 6, 7),
      "6" -> List(1, 2, 4, 5, 6, 7),
      "7" -> List(1, 3, 6),
      "8" -> List(1, 2, 3, 4, 5, 6, 7),
      "9" -> List(1, 2, 3, 4, 6, 7) // why did I 1-index these?
    )

    def patternSegmentSettingGen(displaySetting: Setting): List[(Pattern, String, Setting)] =
      segmentSettingMap.map({ case (k: String, v: List[Int]) =>
        (Pattern(v.map(idx => displaySetting.wiring(idx - 1)).toSet), k, displaySetting)
      }).toList

    val allPossibleSignals =
      inputWiringPermutations
        .map(patternSegmentSettingGen)
        .reduceLeft(_ ++ _)
        .groupBy(_._1)
        .mapValues(_.map({ case (k, num, perm) => (num, perm) }))

    def inferSetting(signal: Signal): Setting = {
      allPossibleSignals
        .filterKeys(signal.patterns.contains(_))
        .map({ case (k: Pattern, v: List[(String, Setting)]) => v })
        .reduceLeft(_ ++ _)
        .groupBy(_._2)
        .mapValues(_.size)
        .filter((m) => m._2 == 10)
        .head
        ._1 // test for results size?
    }

    def decodeOutput(inputSignal: Signal, outputSignal: Signal): Int = {
      val displaySetting = inferSetting(inputSignal)
      val patternSegmentMap = segmentSettingMap
      .map({case (k: String, v: List[Int]) => (Pattern(v.map(idx => displaySetting.wiring(idx - 1)).toSet), k)})
      Integer.parseInt(outputSignal.patterns.map(patternSegmentMap).mkString)
    }

    def decodeAndSumAll(inputSignals: List[Signal], outputSignals: List[Signal]): Int = 
      inputSignals.zip(outputSignals).map({ case (i: Signal, o: Signal) => decodeOutput(i, o)}).sum
  }
}
