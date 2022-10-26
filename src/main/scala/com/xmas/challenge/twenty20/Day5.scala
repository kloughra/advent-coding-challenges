package com.xmas.challenge.twenty20

import scala.io.Source

/**
 * --- Day 5: Binary Boarding ---
 */
object Day5 {

  private lazy val day5File: String = "Day5_2020_Input.txt"
  lazy val defaultInput: List[String] = Source.fromResource(day5File).getLines.toList

  def parseSeatOperators(seats: List[String]): List[List[String]] = seats.map(_.split("").toList)

  def applyOperator(operator:String, range: (Int, Int)): (Int,Int) = {
    val half: Int = ((range._2 - range._1)/2).asInstanceOf[Float].ceil.toInt
    operator match {
      case "F" ⇒ (range._1, range._1 + half)
      case "B" ⇒ (range._2 - half, range._2)
      case "L" ⇒ (range._1, range._1 + half)
      case "R" ⇒ (range._2 - half, range._2)
    }
  }

  def calculateSeatPosition( seat: List[String]): ( Int, Int ) = {
    val seatAddress: (List[String], List[String]) = seat.splitAt(7)
    val row: Int = seatAddress(0).foldLeft[(Int, Int)]((0, 127))((position, operator) ⇒ applyOperator(operator, position))(0) //todo -  they should be the same number - check
    val col: Int = seatAddress(1).foldLeft[(Int, Int)]((0, 7))((position, operator) ⇒ applyOperator(operator, position))(1) //todo -  they should be the same number - check
    (row, col)
  }
  def calculateUniqueSeatId(position: (Int,Int)): Int = ( position._1 * 8 ) + position._2


  lazy val calculateSeatIdsDefault: List[Int] = parseSeatOperators(defaultInput).map(seat ⇒ calculateUniqueSeatId(calculateSeatPosition(seat)))
  def calculateMaxIdDefault: Int = calculateSeatIdsDefault.max


  /**
   * It's a completely full flight, so your seat should be the only missing boarding pass in your list. However, there's a catch: some of the seats at the very front and back of the plane don't exist on this aircraft, so they'll be missing from your list as well.
   *
   * Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.
   */

  //get all possible seat ids
  lazy val listOfAllPossibleSeatIds: List[Int] = (0 to 127).flatMap(row ⇒ (0 to 7).map( ( row, _ ) )).toList.map(calculateUniqueSeatId)

  //what seats is not in the input
  lazy val seatsNotInList: List[Int] = listOfAllPossibleSeatIds.filterNot( possibleId ⇒ calculateSeatIdsDefault.contains(possibleId))
  lazy val seatsInFrontOrBack: List[Int] = List(0,127).flatMap(row ⇒ (0 to 7).map( ( row, _ ) ).toList).map(calculateUniqueSeatId)

  //from those, which are not in the front or back
  lazy val missingSeats = seatsNotInList.filterNot(seat ⇒ seatsInFrontOrBack.contains(seat))

  //from those, which doesn't have +1 and -1 in the same list
//  lazy val mySeat = missingSeats.foldLeft(Some(-1,-1))((a,b) ⇒ a._1)

}
