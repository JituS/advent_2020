package twenty_twenty

import scala.io.Source

object Problem12 {

  case class Instruction(opcode: String, operand: Int)

  case class Coordinate(x: Int, y: Int) {
    private val ninetyDegreeTransition = Map((-1, -1) -> (-1, 1), (-1, 1) -> (1, 1), (1, 1) -> (1, -1), (1, -1) -> (-1, -1))
    private val degrees = Map(-90 -> 270, -270 -> 90, -180 -> 180)

    def rotate(degree: Int): Coordinate = {
      (1 to degrees.getOrElse(degree, degree) / 90).foldLeft(this) { (result, _) =>
        val swap = (result.x, result.y).swap
        val tuple = ninetyDegreeTransition((if (result.x == 0) 1 else result.x.sign, if (result.y == 0) 1 else result.y.sign))
        Coordinate(Math.abs(swap._1) * tuple._1, Math.abs(swap._2) * tuple._2)
      }
    }
  }

  case class WayPoint(cords: Coordinate) {
    def turn(angle: Int): WayPoint = WayPoint(cords.rotate(angle))
  }

  case class Ship(x: Int, y: Int, wayPoint: WayPoint) {
    private val waypointDirectionMap = Map("E" -> (1, 0), "W" -> (-1, 0), "N" -> (0, 1), "S" -> (0, -1))

    def move(instruction: Instruction): Ship = {
      val opcode = instruction.opcode
      val operand = instruction.operand
      if (opcode == "F") Ship(x + wayPoint.cords.x * operand, y + wayPoint.cords.y * operand, wayPoint)
      else if (Seq("L", "R").contains(opcode)) Ship(x, y, wayPoint.turn(if (opcode == "L") -operand else operand))
      else waypointDirectionMap.get(opcode)
        .map(e => Ship(x, y, WayPoint(Coordinate(wayPoint.cords.x + e._1 * operand, wayPoint.cords.y + e._2 * operand))))
        .getOrElse(this)
    }
  }

  def main(args: Array[String]): Unit = {
    val list = Source.fromInputStream(getClass.getResourceAsStream("problem12")).getLines()
      .map(e => Instruction(e.take(1), e.slice(1, e.length).toInt)).toList
    val result = list.foldLeft(Ship(0, 0, WayPoint(Coordinate(10, 1))))(_ move _)
    println(Math.abs(result.x) + Math.abs(result.y))
  }
}