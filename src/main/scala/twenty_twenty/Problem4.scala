package twenty_twenty

import scala.io.Source

case class Passport(byr: Int, iyr: Int, eyr: Int, hgt: String, hcl: String, ecl: String, pid: String) {
  def isHeightValid: Boolean = {
    val pattern = """^(\d+)([a-z]+)$""".r
    hgt match {
      case pattern(height, unit) if unit == "cm" => height.toInt >= 150 && height.toInt <= 193
      case pattern(height, unit) if unit == "in" => height.toInt >= 59 && height.toInt <= 76
      case _ => false
    }
  }

  def isValid: Boolean = {
    (byr >= 1920 && byr <= 2002) &&
      (iyr >= 2010 && iyr <= 2020) &&
      (eyr >= 2020 && eyr <= 2030) &&
      isHeightValid &&
      hcl.matches("""^#[0-9a-f]{6}$""") &&
      ecl.matches("^(amb|blu|brn|gry|grn|hzl|oth)$") &&
      pid.matches("""^\d{9}$""")
  }
}

object Passport {
  def apply(map: Map[String, String]): Passport = new Passport(map("byr").toInt, map("iyr").toInt, map("eyr").toInt, map("hgt"), map("hcl"), map("ecl"), map("pid"))
}


object Problem4 {
  def main(args: Array[String]): Unit = {
    val validAttr = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid").sorted
    val mandatoryAttr = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").sorted

    val validPassport = Source.fromInputStream(getClass.getResourceAsStream("problem4"))
      .getLines().mkString("\n")
      .split("\n{2}")
      .map(_.split("\\s+").map { keyValue =>
        val keyValueList = keyValue.split(":")
        (keyValueList(0), keyValueList(1))
      }.toMap)
      .filter(e => List(mandatoryAttr, validAttr).contains(e.keys.toList.sorted))
      .map(Passport(_))
      .count(_.isValid)
    println(validPassport)
  }
}
