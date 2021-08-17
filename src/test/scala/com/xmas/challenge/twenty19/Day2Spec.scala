package com.xmas.challenge.twenty19

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {

  behavior of "Day2"

  it should "execute intcode computer" in {
    val intCode1 = Array(1,0,0,0,99)
    Day2.executeIntCode(intCode1) shouldBe Array(2,0,0,0,99)

    val intCode2 = Array(2,3,0,3,99)
    Day2.executeIntCode(intCode2) shouldBe Array(2,3,0,6,99)

    val intCode3 = Array(2,4,4,5,99,0)
    Day2.executeIntCode(intCode3) shouldBe Array(2,4,4,5,99,9801)

    val intCode4 = Array(1,1,1,4,99,5,6,0,99)
    Day2.executeIntCode(intCode4) shouldBe Array(30,1,1,4,2,5,6,0,99)
  }



}
