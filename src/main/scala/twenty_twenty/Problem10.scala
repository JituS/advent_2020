package twenty_twenty

import scala.io.Source

object Problem10 {
  def main(args: Array[String]): Unit = {
    val adaptors = Source.fromInputStream(getClass.getResourceAsStream("problem10")).getLines()
      .map(_.toInt).toList
    val allAdaptors = (adaptors :+ 0 :+ adaptors.max + 3).sorted
    val indexedAdaptors = allAdaptors.zipWithIndex
    val value = getDifferences(indexedAdaptors.tail, indexedAdaptors.head, Map(1 -> 0, 2 -> 0, 3 -> 0))
    println(value)
  }

  def getCount(adaptors: List[(Int, Int)], result: Long = 0): Long = {
    val index = adaptors.map(_._1).zipWithIndex
    index.foldLeft(result) { (result, adaptor) =>
      if (index.slice(adaptor._2, index.length).length > 3 && index(adaptor._2 + 2)._1 - adaptor._1 <= 3) {
        getCount(index.filterNot(_ == index(adaptor._2 + 1)), result + 1)
      } else result
    }
  }

  @scala.annotation.tailrec
  def getDifferences(adaptors: List[(Int, Int)], currentAdaptor: (Int, Int), diffCounts: Map[Int, Int]): Map[Int, Int] = {
    if (adaptors.isEmpty) diffCounts
    else {
      val key = adaptors.head._1 - currentAdaptor._1
      getDifferences(adaptors.tail, adaptors.head, diffCounts + (key -> (diffCounts(key) + 1)))
    }
  }
}
