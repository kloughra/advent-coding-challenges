package com.xmas.challenge.twenty19

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {

  behavior of "Day1"

  it should "calculate fuel" in {
    Day1.calculateFuel(12) shouldBe 2
    Day1.calculateFuel(14) shouldBe 2
    Day1.calculateFuel(1969) shouldBe 654
    Day1.calculateFuel(100756) shouldBe 33583
  }


  it should "calculate fuel required for fuel" in {
    Day1.calculateFuel(12) shouldBe 2
    Day1.calculateFuel(14) shouldBe 2
    Day1.calculateFuel(1969) shouldBe 654
    Day1.calculateFuel(100756) shouldBe 33583
  }

}
