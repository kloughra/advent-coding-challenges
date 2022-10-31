package com.xmas.challenge.twenty21

import scala.io.Source

/**
 * Day 2 - Advent of Code 2021
 * https://adventofcode.com/2021/day/2
 */
object Day2 {

  case class Instruction(direction: String, distance: Int)

  private lazy val day2File: String = "Day2_2021_Input.txt"
  lazy val defaultInput: List[Instruction] = Source.fromResource(day2File)
    .getLines.toList
    .map(i ⇒ {
      val x = i.split(" ")
      Instruction(x.head, x.last.toInt)
    })

  val testInstructions: List[Instruction] = List("forward 5","down 5", "forward 8", "up 3", "down 8", "forward 2")
    .map(i ⇒ {
      val x = i.split(" ")
      Instruction(x.head, x.last.toInt)
    })

  def applyInstruction(instruction: Instruction, depth: Int, horPos: Int): (Int,Int) = instruction.direction match {
    case "forward" ⇒ (depth, horPos + instruction.distance)
    case "down" ⇒ (depth + instruction.distance, horPos)
    case "up" ⇒ (depth - instruction.distance, horPos)
    case _ ⇒  (depth, horPos)
  }
  def applyInstructionWithAim(instruction: Instruction, depth: Int, horPos: Int, aim: Int): (Int,Int,Int) = instruction.direction match {
    case "forward" ⇒ (depth + (instruction.distance * aim), horPos + instruction.distance, aim)
    case "down" ⇒ (depth, horPos, aim + instruction.distance)
    case "up" ⇒ (depth, horPos, aim - instruction.distance)
    case _ ⇒  (depth, horPos, aim)
  }

  def computeInstructions(list: List[Instruction]): (Int,Int) = list.foldLeft((0,0))((tup, instruction) ⇒ applyInstruction(instruction,tup(0), tup(1)))

  def computeInstructionsWithAim(list: List[Instruction]): (Int,Int,Int) = list.foldLeft((0,0,0))((tup, instruction) ⇒ applyInstructionWithAim(instruction,tup(0), tup(1),tup(2)))


}
