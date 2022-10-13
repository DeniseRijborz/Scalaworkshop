object Day01 extends App:
  import scala.io.Source
  import scala.collection.mutable.ListBuffer

  val lines: String =
    Source
      .fromResource("aoc2015/Day01")
      .getLines
      .mkString

  def findfloor(current: Int, instructions: String): Int = 
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

  def positionbasement(current: Int, positions: ListBuffer[Int], instructions: String): Int = 
    if (instructions.isEmpty) {
      val result = positions.toList.indexOf(-1)
      result + 1
    }
    else if (instructions.head.toString == "(") {
      positionbasement(current + 1, positions += current + 1, instructions.tail)
    }
    else if (instructions.head.toString == ")") {
      positionbasement(current - 1, positions += current - 1, instructions.tail)
    }
    else {
      0
    }

  println(findfloor(0,lines))
  println(positionbasement(0,ListBuffer(),lines))
  