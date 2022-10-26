package com.xmas.challenge.twenty19

import scala.annotation.tailrec

object Day4 {
  /**
   *
   * It's a six-digit number.
   * The value is within the range given in your puzzle input.
   * Two adjacent digits are the same (like 22 in 122345).
   * Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
   * Other than the range rule, the following are true:
   *
   * 111111 meets these criteria (double 11, never decreases).
   * 223450 does not meet these criteria (decreasing pair of digits 50).
   * 123789 does not meet these criteria (no double).
   */

    val defaultStart: Int = 147981
    val defaultEnd: Int = 691423

  def passwordCount(start:Int, end: Int): Int = (start to end).count(passwordCriteria)
  def passwordCountWithAdditionalCriteria(start:Int, end: Int): Int = (start to end).count(passwordCriteriaWithAddition)


  def passwordCriteria(password: Int): Boolean = password.toString match {
    case pass if pass.length != 6 ⇒ false
    case candidate ⇒ digitsNeverDecrease(candidate) && containsMatchingAdjacentDigit(candidate)
  }
  def passwordCriteriaWithAddition(password: Int): Boolean = password.toString match {
    case pass if pass.length != 6 ⇒ false
    case candidate ⇒ digitsNeverDecrease(candidate) && containsSingleMatchingAdjacentDigit(candidate)
  }


//  private def password2List(password: String): List[String] = password.split("").toList
  private def digitsNeverDecrease(word: String): Boolean =
    word.split("").foldLeft[(Int, Boolean)]((-1, true))((prev, curr) ⇒ (curr.toInt, !(curr.toInt < prev._1) && prev._2))._2

  private def containsMatchingAdjacentDigit(word: String): Boolean =
    word.split("").foldLeft[(String,Boolean)](("",false))((prev,curr) ⇒
      (curr, (prev._1 == curr) | prev._2 )
    )._2
  //word.matches(".*(\\w)\\1+.*")

//  private def matchingAdjacentDigitCountEven(word:String): Boolean =  word.matches("^((?:.*(\\w)\\2(?!\\2)).+|(\\w)\\3+)$")
  def containsSingleMatchingAdjacentDigit(word:String):Boolean = findRepeats(word.split("").toList)
  @tailrec
  def findRepeats(word: List[String], acc:List[String] = Nil, prev: String = ""): Boolean = word match {
    case Nil ⇒ (acc :+ prev).groupBy(_.length).get(2).exists(_.nonEmpty)
    case c :: tail ⇒ prev match {
      case "" ⇒ findRepeats(tail,acc,c)
      case p ⇒
        if(c.head == p.head) findRepeats(tail,acc,p + c)
        else findRepeats(tail,acc :+ p, c)
    }
  }



}
