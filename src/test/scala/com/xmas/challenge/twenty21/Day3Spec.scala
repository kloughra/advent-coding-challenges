package com.xmas.challenge.twenty21

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
class Day3Spec extends AnyFlatSpec with Matchers {

  behavior of "Day 3"

  val testBinaries = List("00100", "11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010")

  it should "calculate power consumption rate from diagnostic report" in {
    Day3.calculateGammaRate(testBinaries) * Day3.calculateEpsilonRate(testBinaries) shouldBe 198
  }

  it should "calculate the gamma rate from the most common bit in each position" in {
    Day3.calculateGammaRate(testBinaries) shouldBe 22
  }

  it should "calculate the epsilon rate from the least common bit in each position" in {
    Day3.calculateEpsilonRate(testBinaries) shouldBe 9
  }

  it should "calculate the oxygen generator rating" in {
    Day3.calculateOxygenGeneratorRating(testBinaries) shouldBe 23
  }

  it should "calculate the CO2 scrubber rating" in {
    Day3.calculateC02ScrubberRating(testBinaries) shouldBe 10
  }

}
