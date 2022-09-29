object DayTwo extends App:

  import scala.io.Source

  val lines: List[String] = Source.fromFile("aoc\\src\\resources\\aoc2018\\Day02").getLines.toList

  def countchar(id: String): List[Int] = {
    id.groupBy(identity).map((_, x) => x.length).toList
  }

  def num(l: List[String]): Int = {
    val count: List[List[Int]] = l.map(countchar)
    val twos: Int = count.count(_ contains 2)
    val threes: Int = count.count(_ contains 3)
    twos * threes
  }

  def check(str1: String, str2: String, chars: String = ""): String = {
    if (str1.isEmpty) {
      chars
    }
    else {
      if (str1.head == str2.head) {
        check(str1.tail, str2.tail, chars + str1.head)
      }
      else
        //if (str1.head != str2.head)
        {
        check(str1.tail, str2.tail, chars)
      }
    }
  }

  def check2(str1: String, str2: String, count: Int = 0): Int = {
    val newcount = count
    if (str1.isEmpty) {
      newcount
    }
    else {
      if (str1.head == str2.head) {
        check2(str1.tail, str2.tail, count)
      }
      else
      //if (str1.head != str2.head)
      {
        val newcount: Int = count + 1
        check2(str1.tail, str2.tail, newcount)
      }
    }
  }
  for (word <- lines)
    for (word2 <- lines.tail)
      if (check2(word,word2) == 1) {
        println(word)
        println(word2)
      }


  println(num(lines))
  println(check("fzvstwblgqkhpuixdrnevmaycd", "fivstwblgqkhpuixdrnevmaycd"))