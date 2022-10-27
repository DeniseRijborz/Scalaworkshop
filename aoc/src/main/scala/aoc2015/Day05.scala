package aoc2015

import scala.io.Source

object Day05 extends App:

  val lines: List[String] =
    Source
      .fromResource("aoc2015/Day05")
      .getLines
      .toList

  def doubleLetterInRow(x: String): Boolean =
    if x.slice(0, 1) == x.slice(1, 2) then true
    else false

  val answer1: Int =
    val nice = for {
      s <- lines
      if s.toList.count("aeiou".toList.contains) >= 3
      if s.sliding(2).toList.map(x => doubleLetterInRow(x)).contains(true)
      if !s.contains("ab") && !s.contains("cd") && !s.contains("pq") && !s.contains("xy")
    } yield s
    nice.size

  def repeatLetterWithLetterInBetween(x: String): Boolean =
    if x.slice(0, 1) == x.slice(2, 3) then true
    else false

  def containsPairTwice(s: String): Boolean =
    val pairs = s.sliding(2).toList.filter(_.length > 1)
    if pairs.groupBy(identity).filter(_._2.length > 1).nonEmpty then
      val letters: String = pairs.groupBy(identity).filter(_._2.length > 1).keys.head
      val index1: Int = s.indexOf(letters)
      val index2: Int = s.lastIndexOf(letters)
      if index1 + 1 == index2 then false
      else true
    else false

  def containsRepeatLetter(s: String): Boolean =
    if s.sliding(3).toList.map(x => repeatLetterWithLetterInBetween(x)).contains(true) then true
    else false

  def answer2(file: List[String]): Int =
    val nice = file.map(s => containsPairTwice(s) && containsRepeatLetter(s))
    nice.count(_ == true)

  println(answer1)
  println(answer2(lines))
