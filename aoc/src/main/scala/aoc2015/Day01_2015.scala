object Day01_2015 extends App:
  import scala.io.Source

  val lines = Source.fromFile("aoc\\src\\resources\\aoc2015\\Day01_2015").getLines.mkString

  def findfloor(current: Int, instructions: String): Int = {
    if (instructions.isEmpty) {
      current
    }
    else if (instructions.head.toString == "(") {
      findfloor(current + 1, instructions.tail)
    }
    else if (instructions.head.toString == ")") {
      findfloor(current - 1, instructions.tail)
    }
    else {
      0
    }
  }

  println(findfloor(0,lines))