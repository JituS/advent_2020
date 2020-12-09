package twenty_twenty

import scala.io.Source

object Problem9 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromInputStream(getClass.getResourceAsStream("problem9")).getLines()
      .map(_.toLong).toList.zipWithIndex
    val preamble = 25
    val result = numbers.slice(preamble, numbers.length).find(summedResultOfTwoPreviousValues(_, numbers, preamble).isEmpty).map(_._1).get
    println(result) // part one

    numbers.foldLeft(List[Long]()) { (results, number) =>
      val numberList = numbers.slice(number._2, numbers.length)
      val consecutiveNumbers = getConsecutiveNumbers(numberList.tail, List(numberList.head), result).map(_._1).filterNot(_ == result)
      if (consecutiveNumbers.sum == result) results :+ consecutiveNumbers.min + consecutiveNumbers.max
      else results
    }.foreach(println) // part two
  }

  @scala.annotation.tailrec
  def getConsecutiveNumbers(list: List[(Long, Int)], consecutiveNumbers: List[(Long, Int)], summedUpNumber: Long, currentSum: Long = 0L): List[(Long, Int)] = {
    if (list.nonEmpty && currentSum + list.head._1 < summedUpNumber && list.head._2 == consecutiveNumbers.last._2 + 1)
      getConsecutiveNumbers(list.tail, consecutiveNumbers :+ list.head, summedUpNumber, currentSum + list.head._1)
    else consecutiveNumbers
  }

  @scala.annotation.tailrec
  def matchGivenNumber(dataList: List[Long], number1: Option[Long], number2: Long, numberToMatch: Long): Option[Long] = {
    val maybeInt = dataList.find(e => e + number1.getOrElse(0L) + number2 == numberToMatch).map(_ * number1.getOrElse(1L) * number2)
    if (dataList.nonEmpty && maybeInt.isEmpty) matchGivenNumber(dataList.tail, number1, dataList.head, numberToMatch)
    else maybeInt
  }

  private def summedResultOfTwoPreviousValues(number: (Long, Int), numbers: List[(Long, Int)], preamble: Int) = {
    val list = numbers.slice(number._2 - preamble, number._2).map(_._1)
    matchGivenNumber(list.tail, None, list.head, number._1)
  }
}