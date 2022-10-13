package aoc2015

import scala.io.Source

object Day02 extends App:

  case class Box(w: Int, l: Int, h: Int):

    val wlArea: Int =
      w * l

    val whArea: Int =
      w * h

    val lhArea: Int =
      l * h

    val area: Int =
      2 * wlArea + 2 * whArea + 2 * lhArea

    val volume: Int =
      l * w * h

    val wrapMargin: Int =
      List(wlArea, whArea, lhArea).min

    val wrapArea: Int =
      area + wrapMargin

    val shortestPerimeter: Int =
      List(w + l, w + h, l + h).map(_ * 2).min

    val ribbonLength: Int =
      val bow = volume
      shortestPerimeter + bow


  object Box:

    def fromString(s: String): Box =
      s.trim match
        case s"${w}x${l}x${h}" => Box(w.toInt, l.toInt, h.toInt)
        case _                 => sys.error(s"unknown format $s")


  val boxes: List[Box] =
    Source
      .fromResource("aoc2015/Day02")
      .getLines
      .map(Box.fromString)
      .toList

  def amountOfPaper(boxes: List[Box]): Int =
    boxes.map(_.wrapArea).sum

  def amountOfRibbon(boxes: List[Box]): Int =
    boxes.map(_.ribbonLength).sum

  val answer1: Int =
    amountOfPaper(boxes)

  assert(answer1 == 1588178)

  println(answer1)

  val answer2: Int =
    amountOfRibbon(boxes)

  assert(answer2 == 3783758)

  println(answer2)
