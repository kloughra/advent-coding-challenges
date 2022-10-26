package com.xmas.challenge.twenty20

import scala.io.Source

/**
 * --- Day 6: Custom Customs ---
 */
object Day6 {

  private lazy val day6File: String = "Day6_2020_Input.txt"
  lazy val defaultInput: String = Source.fromResource(day6File).getLines.toList.reduce(_+ '\n' +_)

  def parseCustomForms(input: String) : List[String] =
    input.split("\\n\\n").toList//.map( entry ⇒ entry.split(Array('\n')).toList)

  val containsNewLine: String ⇒ Boolean = str ⇒ Array(" ","\n").contains(str)
  val stringToKey: String ⇒ String = key ⇒ key
//  private val listToCount = { case (k: String,v: List[String]) ⇒ (k,v.length)}

  def groupFormsByAnswers(customForms: List[String]): List[Map[String,Int]] =
    customForms.map(_.split("").filterNot(containsNewLine).groupBy(stringToKey).map({ case (k,v) ⇒ (k,v.length)}))

  def groupFormsByAnswersPerPerson(customForms: List[String]): List[Map[String,Int]] = {
    customForms.map(party ⇒
      party.split("\n").toList
        .flatMap( _.split(""))
        .filterNot(str ⇒ Array(" ","\n").contains(str)).groupBy(k ⇒ k).map({ case (k,v) ⇒ (k,v.length)})
    )
  }

  def countPartyTotals(customUniqueAnswers: List[Map[String,Int]] ): List[Int] = customUniqueAnswers.map(_.keys.size)

  lazy val sumUniqueAnswersDefault: Int = countPartyTotals(groupFormsByAnswers(parseCustomForms(defaultInput))).sum

}
