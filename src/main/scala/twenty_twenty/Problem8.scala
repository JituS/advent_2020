package twenty_twenty

import scala.io.Source

case class Instruction(opCode: String, operand: Int, var executed: Boolean = false) {
  def swap(): Instruction = {
    val strings = List("nop", "jmp")
    Instruction(strings((strings.indexOf(opCode) + 1) % strings.length), operand)
  }

  def execute(accumulator: Int): Int = {
    executed = true
    opCode match {
      case "nop" | "jmp" => accumulator
      case "acc" => accumulator + operand
    }
  }

  def nextStep(): Int = {
    opCode match {
      case "jmp" => operand
      case "acc" | "nop" => +1
    }
  }
}

object Instruction {
  def apply(instruction: String): Instruction = {
    val regex = """^(acc|jmp|nop) ([-+]\d+)$""".r
    val regex(opcode, operand) = instruction
    Instruction(opcode, operand.toInt)
  }
}

object Problem8 {
  def getInstructions: Array[(Instruction, Int)] = {
    Source.fromInputStream(getClass.getResourceAsStream("problem8")).getLines().map(Instruction(_)).toArray.zipWithIndex
  }

  def main(args: Array[String]): Unit = {
    println(executeInstruction(getInstructions, 0, 0)) // part one
    println(fixAndExecuteCode(getInstructions, 0)) // part two
  }

  def swapInstruction(instructions: Array[(Instruction, Int)], index: Int): Array[(Instruction, Int)] = {
    instructions.map { instruction_index: (Instruction, Int) =>
      if (instruction_index._2 == index) (instruction_index._1.swap(), instruction_index._2)
      else instruction_index
    }
  }

  @scala.annotation.tailrec
  def fixAndExecuteCode(instructions: Array[(Instruction, Int)], index: Int): Int = {
    val (result, exitCode) = executeInstruction(swapInstruction(getInstructions, index))
    if (exitCode != 0) fixAndExecuteCode(instructions, index + 1) else result
  }

  @scala.annotation.tailrec
  def executeInstruction(instructions: Array[(Instruction, Int)], index: Int = 0, accumulator: Int = 0): (Int, Int) = {
    val instruction = instructions(index)._1
    if (instruction.executed) (accumulator, 1)
    else if (index == instructions.length - 1) (instruction.execute(accumulator), 0)
    else executeInstruction(instructions, index + instruction.nextStep(), instruction.execute(accumulator))
  }
}
