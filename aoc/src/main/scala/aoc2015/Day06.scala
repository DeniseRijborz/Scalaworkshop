package aoc2015

import scala.io.Source

object Day06 extends App:

  case class Instructions(cmd: String, x0: Int, y0: Int, x1: Int, y1: Int):
    def within(x: Int, y: Int): Boolean =
      x0 <= x && x1 >= x && y0 <= y && y1 >= y

  object Instructions:

    def fromString(s: String): Instructions =
      s match
        case s"toggle $x0,$y0 through $x1,$y1"    => Instructions("toggle", x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case s"turn on $x0,$y0 through $x1,$y1"   => Instructions("on", x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case s"turn off $x0,$y0 through $x1,$y1"  => Instructions("off", x0.toInt, y0.toInt, x1.toInt, y1.toInt)

  val instructions: List[Instructions] =
    Source
      .fromResource("aoc2015/Day06")
      .getLines
      .map(Instructions.fromString)
      .toList

  val answer1: Int =
    val grid: List[List[Boolean]] =
      List.tabulate(1000,1000)((x,y) =>
      instructions
        .filter(_.within(x,y))
        .foldLeft(false)((light,instructions) =>
          instructions.cmd match
            case "toggle" => !light
            case "on" => true
            case "off" => false
        )
      )
    grid
      .flatten
      .count(_ == true)

  println(answer1)

  val answer2: Int =
    val grid: List[List[Int]] =
      List.tabulate(1000, 1000)((x, y) =>
        instructions
          .filter(_.within(x, y))
          .foldLeft(0)((light, instructions) =>
            instructions.cmd match
              case "toggle" => light + 2
              case "on" => light + 1
              case "off" => if light == 0 then light else light - 1
          )
      )
    grid
      .flatten
      .sum

  println(answer2)

