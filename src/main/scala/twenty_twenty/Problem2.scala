package twenty_twenty

import scala.io.Source

case class Config(index1: Int, index2: Int, char: Char, password: String) {
  def meetMisinterpretedPolicy(): Boolean = {
    index1 to index2 contains password.count(e => e.equals(char))
  }

  def meetCorrectPolicy(): Boolean = {
    (password(index1 - 1).equals(char) && !password(index2 - 1).equals(char)) || (!password(index1 - 1).equals(char) && password(index2 - 1).equals(char))
  }
}

object Config {
  def apply(conf: String): Config = {
    val policy :: password :: _ = conf.split(":").toList
    val characterRange :: character :: _ = policy.split(" ").toList
    val min :: max :: _ = characterRange.split("-").map(_.toInt).toList
    Config(min, max, character.head, password.trim)
  }
}


object Problem2 {
  def main(args: Array[String]): Unit = {
    val configs = Source.fromInputStream(getClass.getResourceAsStream("problem2")).getLines().map(Config(_)).toList
    println(configs.count(_.meetMisinterpretedPolicy()))
    println(configs.count(_.meetCorrectPolicy()))
  }
}
