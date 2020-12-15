package twenty_twenty

import scala.collection.mutable

object Problem15 {
  def main(args: Array[String]): Unit = {
    val input = mutable.LinkedHashMap(0 -> List(1), 13 -> List(2), 1 -> List(3), 16 -> List(4), 6 -> List(5), 17 -> List(6))
    (7 to 2020).foldLeft(input) { (result, turn) =>
      val last: (Int, List[Int]) = result.last
      val nextNumber = if (last._2.length == 1) 0 else last._2.last - last._2(last._2.length - 2)
      if (result.contains(nextNumber)) {
        val option: List[Int] = result.remove(nextNumber).get
        result.update(nextNumber, option :+ turn)
        result
      } else {
        result.update(nextNumber, List(turn))
        result
      }
    }
    println(input.last)
  }
}
