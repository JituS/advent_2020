package twenty_twenty

import scala.io.Source

object Problem13 {

  object ordering extends Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = if (x._2 < y._2) 0 else 1

  }

  def partTwo(zipWithIndex: Array[(String, Int)]): Long = {
    val busIds = zipWithIndex.toList.filter(e => e._1 != "x").map(e => (e._1.toLong, e._2))
    val M = busIds.map(_._1).product
    val allA = busIds.map { e => if (e._2 == 0) 0 else e._1.toLong - (e._2 % e._1.toLong) }
    val allM = busIds.map(M / _._1)
    val allm = busIds.map(_._1)
    val allX = allM.zip(allm).map { a => (1 to 300).find(e => (a._1 * e) % a._2 == 1).get }
    allX.zip(allA).map(e => e._1 * e._2).zip(allM).map(e => e._1 * e._2).sum % M
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("problem13")).getLines()
    val estimateArrivalTime = lines.take(1).next().toInt
    val strings = lines.next().split(",")
    val busIds = strings.filterNot(_ == "x").map(_.toInt).toList
    val busWithDeparture: Seq[(Int, Int)] = busIds.map { bus => bus -> (0 to estimateArrivalTime + bus by bus).find(_ >= estimateArrivalTime).get }
    val tuple = busWithDeparture.min(ordering)
    println(tuple._1 * (tuple._2 - estimateArrivalTime)) // part1
    println(partTwo(strings.zipWithIndex)) // part2 - Using chinese remainder theorem
  }
}
