package aoc2015

import scala.io.Source

object Day02_2015 extends App:

  val lines: List[String] = Source.fromFile("aoc\\src\\resources\\aoc2015\\Day02_2015").getLines.toList

  def test(list: List[String]): List[List[String]] = {
    val newlist = list.map(_.split("x").toList)
    newlist
  }

  println(lines)
  println(test(lines))