package twenty_twenty

import scala.io.Source

object Problem6 {
  def main(args: Array[String]): Unit = {
    val result = Source.fromInputStream(getClass.getResourceAsStream("problem6"))
      .getLines().mkString("\n")
      .split("\n{2}")
      .map(groupQuestions => groupQuestions.split("\n").reduce(_ intersect _)).map(_.length)
    println(result.sum)
  }
}
