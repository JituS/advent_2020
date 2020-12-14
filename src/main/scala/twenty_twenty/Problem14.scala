package twenty_twenty


import java.lang.Math.pow

import scala.collection.mutable
import scala.io.Source

object Problem14 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromInputStream(getClass.getResourceAsStream("problem14"))
      .getLines().mkString("\n")
      .split("\n{2}").toList

    val result = input.map { line: String =>
      val bitmask :: values = line.split("\n").toList
      val mask = bitmask.replace("mask = ", "")
      val memValueRegex = """mem\[(\d+)\] = (\d+)""".r
      values.foldLeft(mutable.LinkedHashMap[String, Long]()) { (memValueMap, memValue) =>
        val memValueRegex(mem, value) = memValue
        val memInBinary = toBinary(mem, 36)
        val fluctuatingAdd = mask.zip(memInBinary).map(toFluctuatingAddress).mkString("")
        val noOfX = fluctuatingAdd.count(_ == 'X')
        memValueMap ++ (0 until pow(2, noOfX).toInt)
          .map(e => toBinary(e.toString, noOfX)
            .foldLeft(fluctuatingAdd)((result, bit) => result.replaceFirst("X", bit.toString)) -> value.toLong).toMap
      }
    }
    println(result.reduce(_ ++ _).values.sum)
  }

  private val toFluctuatingAddress: PartialFunction[(Char, Char), Char] = {
    case ('X', _) => 'X'
    case ('0', number) => number
    case ('1', _) => '1'
  }

  private def toBinary(value: String, padding: Int) = {
    ("0" * padding).substring(value.toLong.toBinaryString.length) + value.toLong.toBinaryString
  }
}
