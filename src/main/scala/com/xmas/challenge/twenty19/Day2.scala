package com.xmas.challenge.twenty19

import scala.annotation.tailrec
import scala.io.Source

/**
 * IntCode - List of Integers - Initial State of Computer's Memory
 *
 * To run - Initialize memory to the computer's values
 * Address - position in memory - aka position in list
 *
 * OpCodes - mark the beginning of an Instruction
 * Values following opcode are the Parameters for the Instruction
 * The instruction 99 contains only an opcode and has no parameters.
 *
 * Instruction Pointer - Address of the current instruction
 * After an instruction is completed, the pointer increases by the number of values in the instruction
 *
 */

object Day2 {

  /**
   * An Intcode program is a list of integers separated by commas (like 1,0,0,3,99).
   * To run one, start by looking at the first integer (called position 0).
   * Here, you will find an opcode - either 1, 2, or 99. The opcode indicates what to do; for example, 99 means that the
   * program is finished and should immediately halt. Encountering an unknown opcode means something went wrong.
   *
   *
   * With terminology out of the way, we're ready to proceed.
   * To complete the gravity assist, you need to determine what pair of inputs produces the output 19690720."
   *
   * The inputs should still be provided to the program by replacing the values at
   * addresses 1 and 2, just like before. In this program, the value placed in address 1 is called the noun,
   * and the value placed in address 2 is called the verb. Each of the two input values will be between 0 and 99, inclusive.
   *
   */

  private lazy val day2File: String = "Day2Input.txt"
  lazy val defaultInput: Array[Int] = Source.fromResource(day2File).getLines.reduce(_+_).split(',').map(_.toInt)

  @tailrec
  def applyOpCode(opIndex: Int, intCode: Array[Int]): Array[Int] = intCode(opIndex) match {
    case 99 ⇒ intCode //end
    case 1 ⇒ applyOpCode(step(opIndex), opcode(opIndex,intCode, _ + _))
    case 2 ⇒ applyOpCode(step(opIndex), opcode(opIndex,intCode, _ * _))
    case _ ⇒ throw new Exception("Encountered unknown OpCode")

  }

  /**
   * Opcode 1 adds together numbers read from two positions and stores the result in a third position.
   * The three integers immediately after the opcode tell you these three positions
   *  - the first two indicate the positions from which you should read the input values,
   *    and the third indicates the position at which the output should be stored.
   * Opcode 2 - same as 1, but with multiplication
   *
   *    WARNING - default array is mutable
   */
  def opcode(index: Int, intCode: Array[Int], method: (Int,Int) ⇒ Int): Array[Int] = {
    val i1 : Int = intCode(index + 1)
    val i2: Int = intCode(index + 2)
    val i3: Int = intCode(index + 3)
    intCode.update(i3, method.apply(intCode(i1),intCode(i2)))
    intCode
  }

  def step(index: Int): Int = index + 4

  def executeIntCode(intCode: Array[Int]): Array[Int] = {
    applyOpCode(0,intCode)
  }

  def applyGravity(intCode: Array[Int]): Array[Int] = {
    intCode.update(1,12)
    intCode.update(2,2)
    intCode
  }
  def executeIntCodeWithGravity(intCode: Array[Int]): Array[Int] = {
    executeIntCode(applyGravity(intCode))
  }

  def executeDefaultIntCode: Int = executeIntCodeWithGravity(defaultInput)(0)

}

/**
 * Part 2
 * Applying new Terminology
 *
 * Determine what pair of inputs produces the output 19690720
 */
object IntCodeComputer {

  def getDefaultMemory: Array[Int] = Source.fromResource("Day2Input.txt").getLines.reduce(_+_).split(',').map(_.toInt)

  private def applyInstruction(instructionPointer: Int, memory: Array[Int]): Array[Int] = memory(instructionPointer) match {
    case 99 ⇒ memory //end
    case 1 ⇒ applyInstruction(step(instructionPointer), instruction(instructionPointer,memory, _ + _))
    case 2 ⇒ applyInstruction(step(instructionPointer), instruction(instructionPointer,memory, _ * _))
    case _ ⇒ throw new Exception("Encountered unknown Instruction")
  }

  /**
   * Opcode 1 adds together numbers read from two positions and stores the result in a third position.
   * The three integers immediately after the opcode tell you these three positions
   *  - the first two indicate the positions from which you should read the input values,
   *    and the third indicates the position at which the output should be stored.
   * Opcode 2 - same as 1, but with multiplication
   *
   *    WARNING - default array is mutable
   */
  private def instruction(instructionPointer: Int, memory: Array[Int], method: (Int,Int) ⇒ Int): Array[Int] = {
    val p1 : Int = memory(instructionPointer + 1)
    val p2: Int = memory(instructionPointer + 2)
    val p3: Int = memory(instructionPointer + 3)
    memory.update(p3, method.apply(memory(p1),memory(p2)))
    memory
  }

  private def step(index: Int): Int = index + 4

  def initialize(noun:Int, verb:Int): Array[Int] = {
    val initialMemory: Array[Int] = getDefaultMemory
    //todo check if noun and verb are between 0 and 99 inclusive
    initialMemory.update(1,noun)
    initialMemory.update(2,verb)
    initialMemory
  }

  def execute(memory: Array[Int]): Int = applyInstruction(0, memory)(0)

  def findInput(output: Int): Int = {

    val validInput: Seq[Option[(Int,Int)]] = for {
      noun ← 0 to 99
      verb ← 0 to 99
    } yield {
      if ( execute(initialize(noun,verb)) == output ) Some((noun,verb))
      else None
    }

    validInput.find(_.isDefined).flatten match {
      case Some((noun,verb)) ⇒
        println(s"Noun and Verb: $noun, $verb")
        (100 * noun) + verb
      case None ⇒
        throw new Exception(s"No noun and verb combination between 0 and 99 exist for output $output")
    }

  }
}