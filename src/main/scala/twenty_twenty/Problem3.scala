package twenty_twenty

import scala.io.Source

object Problem3 {
  def main(args: Array[String]): Unit = {
    val twoDimensionalPath = Source.fromInputStream(getClass.getResourceAsStream("problem3"))
      .getLines().map(_.split("").toList).zipWithIndex.toList
    println(getTreesEncountered(twoDimensionalPath, (3, 1))) // part one
    println(List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map(getTreesEncountered(twoDimensionalPath, _).toLong).product) // part two
  }

  private def getTreesEncountered(twoDimensionalPath: List[(List[String], Int)], slope: (Int, Int)) = {
    twoDimensionalPath.filter(e => e._2 % slope._2 == 0)
      .map(_._1).zipWithIndex.flatMap {
      case (row, index) => row((index * slope._1) % row.length)
    }.count(_ == '#')
  }
}
