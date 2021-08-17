package com.xmas.challenge.twenty20

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {

  behavior of "Day 4"

  it should "parse batch passwords" in {
    val input: String  = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

    println(input.split("\\n\\n").reduce(_ + " \n****\n****\n" + _))

    Day4.parseValidPassports(input).length shouldBe 2
  }


  def validateHeight(hgt: String): Boolean = {
      if(hgt.contains("cm")) (hgt.dropRight(2).toInt >= 150) && (hgt.dropRight(2).toInt <= 193)
      else if(hgt.contains("in")) (hgt.dropRight(2).toInt >= 59) && (hgt.dropRight(2).toInt <= 76)
      else false
  }

  def validateEyeColor(ecl: String): Boolean = {
    List("amb","blu","brn","gry","grn","hzl","oth").contains(ecl)
  }

  def validateHairColor(hcl: String): Boolean = {
    val hairColorPattern = "#([\\d|a-f]{6})".r
    hairColorPattern.findFirstIn(hcl) match {
      case Some(_) ⇒ true
      case None ⇒ false
    }
  }

  def validatePID(pid: String): Boolean = {
    val pidPattern = "(\\d{9})".r
    pidPattern.findFirstIn(pid) match {
      case Some(_) ⇒ {
        println(s"PID: $pid")
        pid.length == 9
      }
      case None ⇒ {
        println(s"BAD PID: $pid")
        false
      }
    }
  }
  it should "validate passport fields" in {
    val validByr = "2002"
    (validByr.length == 4) && (validByr.toInt >= 1920) && (validByr.toInt <= 2002) shouldBe true

    val invalidByr = "2003"
    (invalidByr.length == 4) && (invalidByr.toInt >= 2010) && (invalidByr.toInt <= 2020) shouldBe false

    val validHgt = "60in"
    val validHgt2 = "190cm"
    val invalidHgt = "190in"
    val invalidHgt2 = "190"

    validateHeight(validHgt) shouldBe true
    validateHeight(validHgt2) shouldBe true
    validateHeight(invalidHgt) shouldBe false
    validateHeight(invalidHgt2) shouldBe false

    val validEcl = "brn"
    val invalidEcl = "wat"

    validateEyeColor(validEcl) shouldBe true
    validateEyeColor(invalidEcl) shouldBe false

    val validHcl = "#123abc"
    val invalidHcl = "#123abz"
    val invalidHcl2 = "123abc"

    validateHairColor(validHcl) shouldBe true
    validateHairColor(invalidHcl) shouldBe false
    validateHairColor(invalidHcl2) shouldBe false

    val validPid = "000000001"
    val invalidPid = "0123456789"

    validatePID(validPid) shouldBe true
    validatePID(invalidPid) shouldBe false


  }


}
