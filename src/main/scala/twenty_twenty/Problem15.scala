package twenty_twenty

import scala.collection.mutable

object Problem15 {
  def main(args: Array[String]): Unit = {
    val numberWithSpokenTurn = mutable.LinkedHashMap(0 -> List(1), 13 -> List(2), 1 -> List(3), 16 -> List(4), 6 -> List(5), 17 -> List(6))
    (7 to 30000000).foreach { turn =>
      val lastSpoken = numberWithSpokenTurn.last._2
      val currentSpoken = if (lastSpoken.length == 1) 0 else lastSpoken.last - lastSpoken(lastSpoken.length - 2)
      numberWithSpokenTurn.update(currentSpoken, (numberWithSpokenTurn.remove(currentSpoken).getOrElse(List()) :+ turn).takeRight(2))
    }
    println(numberWithSpokenTurn.last)
  }
}
