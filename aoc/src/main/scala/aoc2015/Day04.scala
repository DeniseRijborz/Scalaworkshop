package aoc2015

object Day04 extends App:

  import java.security.MessageDigest

  def hash(s: String) = {
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    new java.math.BigInteger(1, m.digest())
      .toString(16)
      .reverse.padTo(32, "0")
      .reverse.mkString
  }

  def findhash(numbers: List[Int]): List[List[String]] =
    val combinations: List[List[String]] =
      numbers
        .permutations
        .toList
        .map(x => "abcdef" + x.mkString(""))
        .map(x => List(x,hash(x)))
//        .filter(_.startsWith("00000"))
    combinations

    //val newcombi = combinations.filter(_.startsWith("00000"))
  println(hash("abcdef609043"))
  println(findhash(List(0,1,2,3,4,5,6,7,8,9)))