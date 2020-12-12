package twenty_twenty

import scala.io.Source

object Problem12 {

  case class Instruction(opcode: String, operand: Int)

  case class Direction(value: String) {
    private val directionsCLockWise = List("E", "S", "W", "N")
    private val directionsCounterCLockWise = directionsCLockWise.reverse

    def turn(side: String, angle: Int): Direction = {
      side match {
        case "R" => Direction(directionsCLockWise((directionsCLockWise.indexOf(value) + angle / 90) % directionsCLockWise.length))
        case "L" => Direction(directionsCounterCLockWise((directionsCounterCLockWise.indexOf(value) + angle / 90) % directionsCounterCLockWise.length))
      }
    }
  }

  case class Ship(x: Int, y: Int, currentDirection: Direction) {
    def move(instruction: Instruction): Ship = {
      if (instruction.opcode == "S" || (currentDirection.value == "S" && instruction.opcode == "F")) Ship(x + instruction.operand, y, currentDirection)
      else if (instruction.opcode == "E" || (currentDirection.value == "E" && instruction.opcode == "F")) Ship(x, y + instruction.operand, currentDirection)
      else if (instruction.opcode == "W" || (currentDirection.value == "W" && instruction.opcode == "F")) Ship(x, y - instruction.operand, currentDirection)
      else if (instruction.opcode == "N" || (currentDirection.value == "N" && instruction.opcode == "F")) Ship(x - instruction.operand, y, currentDirection)
      else Ship(x, y, currentDirection.turn(instruction.opcode, instruction.operand))
    }
  }

  def main(args: Array[String]): Unit = {
    val list = Source.fromInputStream(getClass.getResourceAsStream("problem12")).getLines().map(e => Instruction(e.take(1), e.slice(1, e.length).toInt)).toList
    val result = list.foldLeft(Ship(0, 0, Direction("E"))) { (ship, instruction) =>
      ship.move(instruction)
    }
    println(result)
  }
}