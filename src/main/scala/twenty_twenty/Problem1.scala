package twenty_twenty

import scala.io.Source

object Problem1 {
  def main(args: Array[String]): Unit = {
    val dataList = Source.fromInputStream(getClass.getResourceAsStream("problem1")).getLines().map(_.toInt).toList
    println(calculatePartOne(dataList.tail, None, dataList.head))
    println(calculatePartTwo(dataList.tail, dataList.head, dataList.tail.head))
  }

  @scala.annotation.tailrec
  def calculatePartOne(dataList: List[Int], number1: Option[Int], number2: Int): Option[Int] = {
    val maybeInt = dataList.find(e => e + number1.getOrElse(0) + number2 == 2020).map(_ * number1.getOrElse(1) * number2)
    if (dataList.nonEmpty && maybeInt.isEmpty) calculatePartOne(dataList.tail, number1, dataList.head)
    else maybeInt
  }

  def calculatePartTwo(dataList: List[Int], number1: Int, number2: Int): Int = {
    calculatePartOne(dataList, Some(number1), number2).getOrElse(calculatePartTwo(dataList.tail, number2, dataList.head))
  }
}