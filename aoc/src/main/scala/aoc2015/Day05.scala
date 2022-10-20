package aoc2015

import scala.io.Source

object Day05 extends App:

  val lines: String =
    Source
      .fromResource("aoc2015/Day05")
      .getLines
      .mkString