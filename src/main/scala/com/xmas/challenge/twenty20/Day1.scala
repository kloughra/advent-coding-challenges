package com.xmas.challenge.twenty20

import scala.io.Source

/**
 * Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
 * Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
 *
 * For example, suppose your expense report contained the following:
 * 1721
 * 979
 * 366
 * 299
 * 675
 * 1456
 * In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
 *
 * Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
 */

object Day1 {

  private lazy val day1File: String = "Day1_2020_Input.txt"
  lazy val defaultInput: List[Int] = Source.fromResource(day1File).getLines.toList.map(_.toInt)

  //options - either take the first two that match

  private def getCombos(list: List[Int], acc: List[(Int,Int)]): List[(Int, Int)] = list match {
    case Nil ⇒ acc
    case head :: tail ⇒ getCombos(tail, acc ::: tail.map( x ⇒ (head,x)))
  }

  private def getTriples(list: List[Int], acc: List[(Int,Int,Int)]): List[(Int, Int,Int)] = list match {
    case Nil ⇒ acc
    case head :: tail ⇒ getTriples(tail, acc ::: getCombos(tail,Nil).map( pair ⇒ (head, pair._1, pair._2)) )
  }

//  private def findPairMatch(source: List[Int], fx: (Int, Int) ⇒ Boolean ): Option[(Int,Int)] = source.map( x ⇒ source.find(fx.apply(_,x))).filter(_.nonEmpty).map(_.get) match {
//      case Nil ⇒ None
//      case a :: b :: Nil ⇒ Some(a,b)
//      case _ ⇒ None
//    }


  def find2020Match(source: List[Int]): List[(Int,Int)] = getCombos(source,Nil)
    .filter(pair ⇒ (pair._1 + pair._2) == 2020)

  def findDefault:List[Int] = find2020Match(defaultInput).map(tup ⇒ tup._1 * tup._2)

  def find2020MatchTriple(source: List[Int]): List[(Int,Int,Int)] = getTriples(source,Nil)
    .filter(triple ⇒ (triple._1 + triple._2 + triple._3) == 2020)

  def findTripleDefault:List[Int] = find2020MatchTriple(defaultInput).map(tup ⇒ tup._1 * tup._2 * tup._3)


}

