package com.xmas.challenge.twenty19

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {

  behavior of "Day3"

  it should "calculate shortest distances" in {

    val wireA = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(',').toList
    val wireB = "U62,R66,U55,R34,D71,R55,D58,R83".split(',').toList

    Day3.calculateShortestDistance(wireA,wireB) shouldBe 159


    val wireC = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(',').toList
    val wireD = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(',').toList


    Day3.calculateShortestDistance(wireC,wireD) shouldBe 135

  }

  it should "calculate fewest steps " in {

    val wireA = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(',').toList
    val wireB = "U62,R66,U55,R34,D71,R55,D58,R83".split(',').toList

    Day3.calculateFewestSteps(wireA,wireB) shouldBe 610


    val wireC = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(',').toList
    val wireD = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(',').toList


    Day3.calculateFewestSteps(wireC,wireD) shouldBe 410

  }


}
