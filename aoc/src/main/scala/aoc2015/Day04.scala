package aoc2015

object Day04 extends App:

  import java.security.MessageDigest

  def hash(s: String): String = {
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    new java.math.BigInteger(1, m.digest())
      .toString(16)
      .reverse.padTo(32, "0")
      .reverse.mkString
  }

  def findhash(input: String, zeroes: String, number: Int): Int =
    if hash(input + number).startsWith(zeroes) then number
    else findhash(input, zeroes, number + 1)

  println(findhash("yzbqklnj","00000",0))
  println(findhash("yzbqklnj","000000",0))
