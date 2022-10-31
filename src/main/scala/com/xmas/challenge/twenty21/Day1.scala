package com.xmas.challenge.twenty21

import scala.io.Source

/**
 * Day 1 - Advent of Code 2021
 * https://adventofcode.com/2021/day/1
 */
object Day1 {

  private lazy val day1File: String = "Day1_2021_Input.txt"
  lazy val defaultInput: List[Int] = Source.fromResource(day1File).getLines.toList.map(_.toInt)

  trait Delta
  case object Increased extends Delta
  case object Decreased extends Delta
  case object NA extends Delta
  val testList: List[Int] = List(199,200,208,210,200,207,240,269,260,263)

  def countIncreasedDepths(depthList: List[Int]): Int = countIncreased(traverseIntList(depthList))
  def countIncreasedDepthsWindowed(depthList: List[Int], windowSize: Int = 3): Int = countIncreased(traverseIntListWindowed(depthList,windowSize))

  def traverseIntList(list: List[Int]): List[(Int,Delta)] = {
    val comparedList = for (
      i <- list.indices
    ) yield {
      if (i == 0) (list(i), NA)
      else (list(i), if (list(i - 1) > list(i)) Decreased else Increased)
    }
    comparedList.toList
  }

  def traverseIntListWindowed(list: List[Int], windowSize: Int): List[(Int,Delta)] = {
    val comparedList = for (
      i <- list.indices
    ) yield {
      if(i == 0) {
        val sum = (i until i + windowSize).map(list(_)).sum
        (sum, NA)
      } else {
        val windowTop = if( i + windowSize >= list.length) list.length else i + windowSize
        val sum = (i until windowTop).map(list(_)).sum
        val prevWindowTop = if( i + windowSize - 1 >= list.length) list.length else i + windowSize - 1
        val prevSum = (i - 1 until prevWindowTop).map(list(_)).sum
        (sum, if (prevSum > sum) Decreased else if (prevSum == sum) NA else Increased)
      }
    }
    comparedList.toList
  }

  def countIncreased(deltas: List[(Int, Delta)]): Int = deltas.filter(elem ⇒ elem(1) == Increased).length

}
