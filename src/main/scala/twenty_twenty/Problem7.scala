package twenty_twenty

import scala.io.Source

object Problem7 {
  def main(args: Array[String]): Unit = {
    val allBags: Map[String, Map[String, Int]] = Source.fromInputStream(getClass.getResourceAsStream("problem7"))
      .getLines()
      .map { line =>
        val bagColor :: containedBag :: _ = line.split(" bags contain ").toList
        val bagWithCountRegex = """(\d+) ([a-z ]+) bags?[.|,]""".r
        val bagWithCountMap = bagWithCountRegex.findAllIn(containedBag).matchData.map(e => e.group(2) -> e.group(1).toInt).toMap
        (bagColor, bagWithCountMap)
      }.toMap

    println(allBags.keys.count(e => containsBag("shiny gold", allBags(e), allBags))) // part one
    println(countNestedBags(allBags, allBags("shiny gold"))) // part two
  }

  def containsBag(bagName: String, currentBag: Map[String, Int], allBags: Map[String, Map[String, Int]]): Boolean = {
    currentBag.keys.toList.contains(bagName) || currentBag.exists(bag => containsBag(bagName, allBags(bag._1), allBags))
  }

  def countNestedBags(allBags: Map[String, Map[String, Int]], currentBag: Map[String, Int]): Int = {
    currentBag.foldLeft(0) { (result, bag) => result + bag._2 + (bag._2 * countNestedBags(allBags, allBags(bag._1))) }
  }
}

