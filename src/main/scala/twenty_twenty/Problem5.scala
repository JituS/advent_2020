package twenty_twenty

import scala.io.Source

object Problem5 {
  def main(args: Array[String]): Unit = {
    val passports = Source.fromInputStream(getClass.getResourceAsStream("problem5")).getLines().map(_.split("").toList).toList
    val reservedSeats = passports.map { passport =>
      (getRowId((0 to 127).toList, passport.slice(0, 7)) * 8) + getColumnId((0 to 7).toList, passport.slice(7, 10))
    }
    println(reservedSeats.max) // part 1
    (reservedSeats.min to reservedSeats.max).filter(e => !reservedSeats.contains(e)).foreach(println) // part 2
  }

  @scala.annotation.tailrec
  def getRowId(rows: List[Int], code: List[String]): Int = {
    code match {
      case head :: _ if head == "F" => getRowId(rows.splitAt(rows.length / 2)._1, code.tail)
      case head :: _ if head == "B" => getRowId(rows.splitAt(rows.length / 2)._2, code.tail)
      case _ => rows.head
    }
  }

  @scala.annotation.tailrec
  def getColumnId(columns: List[Int], code: List[String]): Int = {
    code match {
      case head :: _ if head == "L" => getColumnId(columns.splitAt(columns.length / 2)._1, code.tail)
      case head :: _ if head == "R" => getColumnId(columns.splitAt(columns.length / 2)._2, code.tail)
      case _ => columns.head
    }
  }
}
