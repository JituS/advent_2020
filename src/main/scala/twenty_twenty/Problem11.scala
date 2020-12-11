package twenty_twenty

import scala.io.Source

object Problem11 {
  def main(args: Array[String]): Unit = {
    val list: Seq[(List[(String, Int)], Int)] = Source.fromInputStream(getClass.getResourceAsStream("problem11")).getLines()
      .map(e => e.split("").zipWithIndex.toList).zipWithIndex.toList
    println(shuffleSeats(list).flatMap(_._1.flatMap(_._1)).count(_ == '#'))
  }

  def getAdjacents(arrangement: Seq[(List[(String, Int)], Int)], row: Int, col: Int): Seq[String] = {
    val rowBackward = row - 1 to 0 by -1
    val rowForward = row + 1 until arrangement.length
    val columnForward = col + 1 until arrangement.head._1.length
    val columnBackward = col - 1 to 0 by -1
    List(rowForward.map((_, col)), rowBackward.map((_, col)), columnForward.map((row, _)), columnBackward.map((row, _)),
      rowForward.zip(columnForward), rowForward.zip(columnBackward), rowBackward.zip(columnForward), rowBackward.zip(columnBackward)
    )
      .flatMap(_.find(seatIndex => Seq("L", "#").contains(arrangement(seatIndex._1)._1(seatIndex._2)._1)).map(e => arrangement(e._1)._1(e._2)._1))
      .filter(_.nonEmpty)
  }

  @scala.annotation.tailrec
  def shuffleSeats(arrangement: Seq[(List[(String, Int)], Int)]): Seq[(List[(String, Int)], Int)] = {
    val newArrangement = arrangement.map { row =>
      val seatStatus = row._1.map { seat: (String, Int) =>
        val adjacent = getAdjacents(arrangement, row._2, seat._2)
        if (seat._1 == "L" && adjacent.count(_ == "#") == 0) ("#", seat._2)
        else if (seat._1 == "#" && adjacent.count(_ == "#") >= 5) ("L", seat._2)
        else seat
      }
      (seatStatus, row._2)
    }
    if (!arrangement.equals(newArrangement)) shuffleSeats(newArrangement)
    else newArrangement
  }
}
