package com.xmas.challenge.twenty19

import scala.io.Source

/**
 * The Elves quickly load you into a spacecraft and prepare to launch.
 *
 * At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper. They haven't determined the amount of fuel required yet.
 *
 * Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.
 *
 * Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract 2.
 * However, that fuel also requires fuel, and that fuel requires fuel, and so on.
 * Any mass that would require negative fuel should instead be treated as if it requires zero fuel;
 * the remaining mass, if any, is instead handled by wishing really hard, which has no mass and is outside the scope of this calculation.
 *
 * So, for each module mass, calculate its fuel and add it to the total.
 * Then, treat the fuel amount you just calculated as the input mass and repeat the process,
 * continuing until a fuel requirement is zero or negative.
 *
 * What is the sum of the fuel requirements for all of the modules on your spacecraft?
 *
 */

object Day1 {

  private lazy val day1File: String = "Day1Input.txt"
  lazy val defaultInput: List[Double] = Source.fromResource(day1File).getLines.toList.map(_.toDouble)

  /**
   * Calculate fuel required for a module - take its mass, divide by three, round down, and subtract 2
   * @param moduleMass
   * @return
   */
  def calculateFuel(moduleMass: Double): Double = (moduleMass / 3).floor - 2

  /**
   * Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract 2.
   * However, that fuel also requires fuel, and that fuel requires fuel, and so on.
   * Any mass that would require negative fuel should instead be treated as if it requires zero fuel;
   * the remaining mass, if any, is instead handled by wishing really hard, which has no mass and is outside the scope of this calculation.
   *
   * @param fuelMass
   * @param fuelAccumulator
   * @return
   */
  def calculateFuelForFuelMass(fuelMass:Double, fuelAccumulator: Double = 0): Double = calculateFuel(fuelMass) match {
      case massIgnore if massIgnore <= 0 ⇒ fuelAccumulator
      case massCalculated ⇒ calculateFuelForFuelMass(massCalculated , fuelAccumulator + massCalculated)
    }


  /**
   * For each module mass, calculate its fuel and add it to the total.
   * Then, treat the fuel amount you just calculated as the input mass and repeat the process,
   * continuing until a fuel requirement is zero or negative.
   * @param moduleMasses
   * @return
   */
  def calculateTotalFuel(moduleMasses: List[Double]): Long =
    moduleMasses
      .map(calculateFuel)
      .map(fuel ⇒ fuel + calculateFuelForFuelMass(fuel))
      .sum
      .toLong

  def calculateDefaultFuel: Long = calculateTotalFuel(defaultInput)

}

