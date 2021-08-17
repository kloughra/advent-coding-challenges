package com.xmas.challenge.twenty20

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {

  behavior of "Day3"

  val inputMap = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#".split('\n').toList

  it should "parse map into Tree Matrix" in {
    Day3.parseMatrix(inputMap).isInstanceOf[Day3.TreeMatrix] shouldBe true
  }

  it should "calculate next location based on slope" in {
    val initLocation = Day3.Location(0,0)
    val treeMatrix: Day3.TreeMatrix = Day3.parseMatrix(inputMap)
    Day3.calculateNextLocationOnMatrix(initLocation,Day3.Slope(3,1), treeMatrix) shouldBe Some(Day3.Location(3,1))

    val nextLocation = Day3.Location(9,3)
    Day3.calculateNextLocationOnMatrix(nextLocation,Day3.Slope(3,1), treeMatrix) shouldBe Some(Day3.Location(1,4))
  }

  it should "calculate space type based on location" in {
    val treeMatrix: Day3.TreeMatrix = Day3.parseMatrix(inputMap)
    val initLocation = Day3.Location(0,0)
    Day3.getSpaceFromMatrix(initLocation, treeMatrix) shouldBe Day3.Free

    val nextLocation = Day3.Location(1,3)
    Day3.getSpaceFromMatrix(nextLocation, treeMatrix) shouldBe Day3.Free

    val nextLocation2 = Day3.Location(1,4)
    Day3.getSpaceFromMatrix(nextLocation2, treeMatrix) shouldBe Day3.Tree
  }

  /**
   * Right 1, down 1.
   * Right 3, down 1. (This is the slope you already checked.)
   * Right 5, down 1.
   * Right 7, down 1.
   * Right 1, down 2.
   * In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.
   */
  it should "calculate the number of trees in the path given a slope" in {

    val slope1: Day3.Slope = Day3.Slope(1,1)
    val slope2: Day3.Slope = Day3.Slope(3,1)
    val slope3: Day3.Slope = Day3.Slope(5,1)
    val slope4: Day3.Slope = Day3.Slope(7,1)
    val slope5: Day3.Slope = Day3.Slope(1,2)
    val treeMatrix: Day3.TreeMatrix = Day3.parseMatrix(inputMap)
    val initLocation = Day3.Location(0,0)

    Day3.countTreesOnRoute(initLocation,slope1,treeMatrix,0) shouldBe 2
    Day3.countTreesOnRoute(initLocation,slope2,treeMatrix,0) shouldBe 7
    Day3.countTreesOnRoute(initLocation,slope3,treeMatrix,0) shouldBe 3
    Day3.countTreesOnRoute(initLocation,slope4,treeMatrix,0) shouldBe 4
    Day3.countTreesOnRoute(initLocation,slope5,treeMatrix,0) shouldBe 2
  }
}
