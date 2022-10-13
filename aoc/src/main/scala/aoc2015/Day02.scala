package aoc2015

import scala.io.Source

object Day02 extends App:

  val lines: List[String] =
    Source
      .fromFile("aoc/src/resources/aoc2015/Day02")
      .getLines
      .toList

  def amountofpaper(dimensions: List[String]): Int =
    dimensions
      .map(_.split("x").toList)
      .map(dimension => {
        val l = dimension(0).toInt
        val w = dimension(1).toInt
        val h = dimension(2).toInt
        val lw = l*w
        val wh = w*h
        val hl = h*l
        val calc = (2*lw + 2*wh + 2*hl) + (lw min wh min hl)
        calc})
      .reduce((a,b) => a+b)

  def amountofribbon(dimensions: List[String]): Int =
    dimensions
      .map(_.split("x").toList)
      .map(dimension => {
        val l = dimension(0).toInt
        val w = dimension(1).toInt
        val h = dimension(2).toInt
        val lwh = l * w * h
        val min = l min w min h
        val max = l max w max h
        val med = (l + w + h) - min - max
        val calc = (min + min + med + med) + lwh
        calc
      })
      .reduce((a, b) => a + b)


  println(lines)
  println(amountofpaper(lines))
  println(amountofribbon(lines))
