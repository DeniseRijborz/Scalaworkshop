package aoc2015

import scala.io.Source

object Day05 extends App:

  val lines: List[String] =
    Source
      .fromResource("aoc2015/Day05")
      .getLines
      .toList

  def double(x: String): Boolean =
    if x.slice(0, 1) == x.slice(1, 2) then true
    else false

  def triple(x: String): Boolean =
    if x.slice(0, 1) == x.slice(2, 3) then true
    else false

  def containsThreeVowels(s: String): Boolean =
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val count = s.toList.count(vowels)
    if count >= 3 then true
    else false

  def containsLetterTwice(s: String): Boolean =
    if s.sliding(2).toList.map(x => double(x)).contains(true) then true
    else false

  def doesNotContainString(s: String): Boolean =
    !s.contains("ab") && !s.contains("cd") && !s.contains("pq") && !s.contains("xy")

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
    if s.sliding(3).toList.map(x => triple(x)).contains(true) then true
    else false

  def checkString(file: List[String]): Int =
    val nice = file.map(s => containsThreeVowels(s) && containsLetterTwice(s) && doesNotContainString(s))
    nice.count(_ == true)

  def checkString2(file: List[String]): Int =
    val nice = file.map(s => containsPairTwice(s) && containsRepeatLetter(s))
    nice.count(_ == true)

  println(checkString(lines))
  println(checkString2(lines))
