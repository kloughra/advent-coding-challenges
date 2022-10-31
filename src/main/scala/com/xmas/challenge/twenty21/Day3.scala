package com.xmas.challenge.twenty21
import scala.io.Source

object Day3 {
  private lazy val day3File: String = "Day3_2021_Input.txt"
  lazy val defaultInput: List[String] = Source.fromResource(day3File)
    .getLines.toList

  def calculatePowerConsumption(report: List[String]): Int = calculateEpsilonRate(report) * calculateGammaRate(report)
  def calculateLifeSupportRating(report: List[String]): Int = calculateOxygenGeneratorRating(report) * calculateC02ScrubberRating(report)

  def calculateGammaRate(report: List[String]): Int = calculateRate(report, compareBitCount(true))
  def calculateEpsilonRate(report: List[String]): Int = calculateRate(report, compareBitCount((false)))

  /**
   * 2) Group bits by index
   * 3) Sort map by index (make sure keys are ordered 0 to n)
   * 4) Map to iterable of list of bits - each list contains all bis of an index
   * 5) Compare bit count and choose min or max bit for each index position
   * 6) Fold min/max chars into a string
   * 7) Parse binary into digit
   * @param report
   * @param getRateBit
   * @return
   */
  private def calculateRate(report: List[String],getRateBit: List[Char] ⇒ Char): Int = {
    val indexedBits = report.flatMap(binary ⇒ binary.zipWithIndex.toList)
    val gammaBinary: String = indexedBits.groupBy((_, i) ⇒ i)
      .toList.sortBy(tup ⇒ tup(0))
      .map({ case (_, v) ⇒ v.map(tup ⇒ tup(0)) })
      .map(getRateBit)
      .foldLeft("")(_+_)
    Integer.parseInt(gammaBinary, 2)
  }

  private def compareBitCount(maxOrMin: Boolean)(bits: List[Char]): Char = {
    val groupedBits = bits.groupBy(identity).map({case (k,v) ⇒ (k,v.length)})
    val size1 = groupedBits.getOrElse('1',0)
    val size0 = groupedBits.getOrElse('0',0)
    if (maxOrMin) {
      if (size1 >= size0) '1' else '0'
    } else {
      if (size1 >= size0) '0' else '1'
    }
  }

  /* life support rating, which can be determined by multiplying the oxygen generator rating by the CO2 scrubber rating.
  */

  def calculateOxygenGeneratorRating(report: List[String]): Int = calculateRating(report, true)
  def calculateC02ScrubberRating(report: List[String]): Int = calculateRating(report, false)

  def calculateRating(report: List[String], minOrMax: Boolean, indexCount: Int = 0): Int = report.length match {
    case 0 ⇒ -1
    case 1 ⇒ Integer.parseInt(report.head,2)
    case _ ⇒
      val getBitsByIndex: List[Char] = report.map(bits ⇒ bits(indexCount))
      val maxBit: Char = compareBitCount(minOrMax)(getBitsByIndex)
      val filteredReport: List[String] = report.filter(bits ⇒ bits(indexCount) == maxBit)
      calculateRating(filteredReport, minOrMax, indexCount + 1)
  }
}
