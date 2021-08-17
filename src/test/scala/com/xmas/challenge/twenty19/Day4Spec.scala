package com.xmas.challenge.twenty19

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {

  behavior of "Day4"


  it should "check rules " in {
    Day4.passwordCriteria(111111) shouldBe true
    Day4.passwordCriteria(223450) shouldBe false
    Day4.passwordCriteria(123789) shouldBe false

    println(s"Num passwords: ${Day4.passwordCount(Day4.defaultStart,Day4.defaultEnd)}")
  }

  it should "check additional rules " in {
    Day4.passwordCriteriaWithAddition(112233) shouldBe true
    Day4.passwordCriteriaWithAddition(123444) shouldBe false
    Day4.passwordCriteriaWithAddition(111122) shouldBe true

    println(s"Num passwords: ${Day4.passwordCountWithAdditionalCriteria(Day4.defaultStart,Day4.defaultEnd)}")
  }


}
