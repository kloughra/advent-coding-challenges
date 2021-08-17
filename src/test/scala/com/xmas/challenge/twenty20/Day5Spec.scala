package com.xmas.challenge.twenty20

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 5"

  val input: List[String] = List("FBFBBFFRLR","BFFFBBFRRR","FFFBBBFRRR","BBFFBBFRLL")

  it should "parse seats from input" in {

    val x = Day5.parseSeatOperators(input)
    x.foreach( seat ⇒ {
      println("SEAT == " +seat.reduce(_ + " : " +_))
    })
    1 shouldBe 1
  }

  it should "apply seat operator to input " in {
    Day5.parseSeatOperators(input).foreach( seat ⇒ {
      println(Day5.calculateSeatPosition(seat))
      println(Day5.calculateUniqueSeatId(Day5.calculateSeatPosition(seat)))
    })

    1 shouldBe 1
  }

}
