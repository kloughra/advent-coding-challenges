package com.xmas.challenge.twenty21

import com.xmas.challenge.twenty21.Day5.{CreateDiagram, Diagram, countDangerousAreas, countOverlappingPoints, createDiagramFromLines, getLineSegmentsFromString, prettyPrintDiagram}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {

  behavior of "Day 5"


  val testInputs: List[String] = List(
    "0,9 -> 5,9", //
    "8,0 -> 0,8",
    "9,4 -> 3,4", //?
    "2,2 -> 2,1", //?
    "7,0 -> 7,4", //
    "6,4 -> 2,0",
    "0,9 -> 2,9", //
    "3,4 -> 1,4", //?
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )
  it should "determine how many points overlap at least two times in a list of lines" in {
    val lines = getLineSegmentsFromString(testInputs)
    val newDiagram = createDiagramFromLines(lines)
    prettyPrintDiagram(newDiagram)
    countOverlappingPoints(newDiagram) shouldBe 5
    countDangerousAreas(lines) shouldBe 5
  }

  it should "determine how many points overlap at least two times in a list of lines including diagonals" in {
    val lines = getLineSegmentsFromString(testInputs)
    val newDiagram = createDiagramFromLines(lines, true)
    prettyPrintDiagram(newDiagram)
    countOverlappingPoints(newDiagram) shouldBe 12
    countDangerousAreas(lines, true) shouldBe 12
  }
}
