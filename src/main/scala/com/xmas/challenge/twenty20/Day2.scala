package com.xmas.challenge.twenty20



import scala.io.Source

/**
 *
 * 1-3 a: abcde
 * 1-3 b: cdefg
 * 2-9 c: ccccccccc
 * Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
 *
 * In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.
 *
 * How many passwords are valid according to their policies?
 */
object Day2 {
  private lazy val day1File: String = "Day2_2020_Input.txt"
  lazy val defaultInput: List[String] = Source.fromResource(day1File).getLines.toList

  case class Policy(min: Int, max: Int, char: String)
  case class PasswordEntry(policy: Policy, password: String) {
    private val charCount: Int = password.count(_.toString == policy.char)
    val isValidOldPolicy: Boolean = (policy.min <= charCount) && (charCount <= policy.max)
    val isValid: Boolean = {
      val pos1: Boolean = password(policy.min - 1).toString == policy.char
      val pos2: Boolean = password(policy.max - 1).toString == policy.char
      (pos1 || pos2) && !(pos1 && pos2)
    }
  }

  private val entryPattern = "(\\d+)-(\\d+) (\\w): (\\w*)".r
  private def parsePasswords(entries: List[String]): List[PasswordEntry] = entries.map( entry ⇒
    entry match {
      case entryPattern(min, max , char, password) ⇒ PasswordEntry(Policy(min.toInt,max.toInt,char),password)
      case invalidPass ⇒
        println(s"INVALID: $invalidPass")
        PasswordEntry(Policy(-1,-1,"N/A"),"N/A")
    })

  def countValidPasswords(entries: List[String]): Int = parsePasswords(entries).count(_.isValid)

  lazy val countValidPasswordsDefault = countValidPasswords(defaultInput)


}
