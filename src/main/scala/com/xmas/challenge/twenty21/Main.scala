package com.xmas.challenge.twenty21

object Main extends App {

  /**
   * Day 1
   */
  println(Day1.countIncreased(Day1.traverseIntList(Day1.testList)))
  println(Day1.countIncreasedDepths(Day1.defaultInput))
  println(Day1.countIncreasedDepthsWindowed(Day1.defaultInput))

  /**
   * Day 2
   */
  println(Day2.computeInstructions(Day2.testInstructions))
  println(Day2.computeInstructions(Day2.defaultInput))
  println(Day2.computeInstructionsWithAim(Day2.testInstructions))
  println(Day2.computeInstructionsWithAim(Day2.defaultInput))

  /**
   * Day 3
   */
  println(Day3.calculatePowerConsumption(Day3.defaultInput))
  println(Day3.calculateLifeSupportRating(Day3.defaultInput))

  /**
   * Day 4
   */
  println(Day4.playBingo(Day4.defaultRules,Day4.defaultBoards))
  println(Day4.playBingoToLose(Day4.defaultRules,Day4.defaultBoards))

  /**
   * Day 5
   */
  println(Day5.countDangerousAreas(Day5.defaultLines))
  println(Day5.countDangerousAreas(Day5.defaultLines, true))
}
