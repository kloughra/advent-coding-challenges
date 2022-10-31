package com.xmas.challenge.twenty21

object Main extends App {

  println(Day1.countIncreased(Day1.traverseIntList(Day1.testList)))
  println(Day1.countIncreasedDepths(Day1.defaultInput))
  println(Day1.countIncreasedDepthsWindowed(Day1.defaultInput))

  println(Day2.computeInstructions(Day2.testInstructions))
  println(Day2.computeInstructions(Day2.defaultInput))
  println(Day2.computeInstructionsWithAim(Day2.testInstructions))
  println(Day2.computeInstructionsWithAim(Day2.defaultInput))
  
  
  println(Day3.calculatePowerConsumption(Day3.defaultInput))
  println(Day3.calculateLifeSupportRating(Day3.defaultInput))
}
