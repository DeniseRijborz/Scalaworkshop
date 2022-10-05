object DayOne extends App:

  import scala.io.Source

  val lines = Source.fromFile("aoc\\src\\resources\\aoc2018\\Day01").getLines.toList.map(_.toString.toInt)

  def add(x: List[Int]): Int = {
    x.sum
  }

  def find(current: Int, change: List[Int], set: Set[Int]): Int = {
    val result = current + change.head
    val newset = set.incl(result)
    if (set contains result){
      result
    }
    else if (change.tail.nonEmpty) {
      //val newset = set.incl(result)
      find(result, change.tail, newset)
    }
    else {
      find(result, lines, newset)
    }
  }

  println(add(lines))
  println(find(0,lines, Set()))

