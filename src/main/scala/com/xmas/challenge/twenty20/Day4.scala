package com.xmas.challenge.twenty20


import scala.io.Source

/**
 * --- Day 4: Passport Processing ---
 *
 *
 * The automatic passport scanners are slow because they're having trouble detecting which passports have all required fields. The expected fields are as follows:
 *
 * byr (Birth Year)
 * iyr (Issue Year)
 * eyr (Expiration Year)
 * hgt (Height)
 * hcl (Hair Color)
 * ecl (Eye Color)
 * pid (Passport ID)
 * cid (Country ID)
 */
object Day4 {
  private lazy val day4File: String = "Day4_2020_Input.txt"
  lazy val defaultInput: String = Source.fromResource(day4File).getLines.toList.reduce(_+ '\n' +_)


  val requiredElements: List[String] = List("byr","iyr","eyr","hgt","hcl","ecl","pid")


  type Passport = Map[String, String]

  def marshalPassport(passportEntry: List[String]): Passport = {
    passportEntry.map( element ⇒ {
      val split = element.split(':')
      (split.head, split.last)
    } ).toMap
  }
  def marshalElements(entry: String): List[String] = entry.split(Array(' ','\n')).toList

  /**
  You can continue to ignore the cid field, but each other field has strict rules about what values are valid for automatic validation:

byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
   */
  private val hairColorPattern = "#([\\d|a-f]{6})".r
  private val pidPattern = "(\\d{9})".r
  def isValidPassport(passport: Passport): Boolean = {
    requiredElements.forall(passport.contains) &&
      passport.get("byr").exists(byr ⇒ (byr.length == 4) && (byr.toInt >= 1920) && (byr.toInt <= 2002)) &&
      passport.get("iyr").exists(iyr ⇒ (iyr.length == 4) && (iyr.toInt >= 2010) && (iyr.toInt <= 2020)) &&
      passport.get("eyr").exists(eyr ⇒ (eyr.length == 4) && (eyr.toInt >= 2020) && (eyr.toInt <= 2030)) &&
      passport.get("hgt").exists(hgt ⇒ {
        if(hgt.contains("cm")) (hgt.dropRight(2).toInt >= 150) && (hgt.dropRight(2).toInt <= 193)
        else if(hgt.contains("in")) (hgt.dropRight(2).toInt >= 59) && (hgt.dropRight(2).toInt <= 76)
        else false
      }) &&
      passport.get("hcl").exists( hcl ⇒ hairColorPattern.findFirstIn(hcl) match {
        case Some(_) ⇒ hcl.length == 7
        case None ⇒ false
      }) &&
      passport.get("ecl").exists( ecl ⇒ List("amb","blu","brn","gry","grn","hzl","oth").contains(ecl)) &&
      passport.get("pid").exists( pid ⇒ pidPattern.findFirstIn(pid) match {
        case Some(_) ⇒ pid.length == 9
        case None ⇒ false
      })



  }

  def parseValidPassports(input: String): List[Passport] = {
    val listEntries: List[String] = input.split("\\n\\n").toList
    println(listEntries.reduce(_ + " \n****\n****\n" + _))
    val entryMaps: List[Passport] = listEntries.map(entry ⇒ marshalPassport(marshalElements(entry)))
    entryMaps.filter(isValidPassport)
  }

  lazy val numValidPassportsDefault: Int = parseValidPassports(defaultInput).length




}
