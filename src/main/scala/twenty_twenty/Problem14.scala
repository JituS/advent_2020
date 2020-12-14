package twenty_twenty


import java.math.BigInteger

import scala.io.Source

object Problem14 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromInputStream(getClass.getResourceAsStream("problem14"))
      .getLines().mkString("\n")
      .split("\n{2}").toList

    val result = input.map { line: String =>
      val bitmask :: values = line.split("\n").toList
      val mask = bitmask.replace("mask = ", "")
      val valueReg = """mem\[(\d+)\] = (\d+)""".r

      values.foldLeft(Map[Int, String]()) { (result, e) =>
        val valueReg(mem, value) = e
        val str = toBinary(value)
        result + (mem.toInt -> str)
      }.map { value: (Int, String) =>
        val str = mask.zip(value._2)
          .map {
            case ('X', number) => number
            case ('0', _) => '0'
            case ('1', _) => '1'
          }.mkString("")
        (value._1, new BigInteger(str, 2).longValue())
      }
    }
    println(result.reduce(_ ++ _).values.sum)
  }

  private def toBinary(value1: String) = {
    "000000000000000000000000000000000000".substring(value1.toLong.toBinaryString.length) + value1.toLong.toBinaryString
  }
}
