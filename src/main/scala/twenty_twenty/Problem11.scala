package twenty_twenty

import scala.io.Source

object Problem11 {
  def main(args: Array[String]): Unit = {
    val list: Seq[(List[(String, Int)], Int)] = Source.fromInputStream(getClass.getResourceAsStream("problem11")).getLines()
      .map(e => e.split("").zipWithIndex.toList).zipWithIndex.toList

    val value = shuffleSeats(list)
    println(value.flatMap(_._1.flatMap(_._1)).count(_ == '#'))
  }

  def getAdjacent(original: Seq[(List[(String, Int)], Int)], row: Int, col: Int): Seq[String] = {
    val (minX, maxX) = (if (row == 0) 0 else row - 1, if (row == original.length - 1) row else row + 1)
    val (minY, maxY) = (if (col == 0) 0 else col - 1, if (col == original.head._1.length - 1) col else col + 1)
    (minX to maxX).flatMap { x =>
      (minY to maxY).map { y =>
        if (x != row || y != col) Some(original(x)._1(y)._1)
        else None
      }
    }.filter(_.isDefined).map(_.get).toList
  }

  @scala.annotation.tailrec
  def shuffleSeats(original: Seq[(List[(String, Int)], Int)]): Seq[(List[(String, Int)], Int)] = {
    val result = original.map { row =>
      val newSymbol = row._1.map { col: (String, Int) =>
        val adjacent = getAdjacent(original, row._2, col._2)
        if (col._1 == "L" && adjacent.count(_ == "#") == 0) ("#", col._2)
        else if (col._1 == "#" && adjacent.count(_ == "#") >= 4) ("L", col._2)
        else col
      }
      (newSymbol, row._2)
    }
    if (!original.equals(result)) shuffleSeats(result)
    else result
  }
}
