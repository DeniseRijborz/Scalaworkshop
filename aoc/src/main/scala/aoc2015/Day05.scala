package aoc2015

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day05 extends App:

  val lines: List[String] =
    Source
      .fromResource("aoc2015/Day05")
      .getLines
      .toList

  def containsThreeVowels(s: String): Boolean =
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val count = s.toList.count(vowels)
    if count > 3 then true
    else false

  def double(x: String): Boolean =
    if x.slice(0,1) == x.substring(1,2) then true
    else false

  def containsLetterTwice(s: String): Boolean =
    if s.sliding(2).toList.map(x => double(x)).contains(true) then true
    else false

  def containsString(s: String): Boolean =
    !s.contains("ab") && !s.contains("cd") && !s.contains("pq") && !s.contains("xy")

  def checkString(file: List[String]): List[((Boolean, Boolean), Boolean)] =
    val one = file.map(s => containsThreeVowels(s))
    val two = file.map(s => containsLetterTwice(s))
    val three = file.map(s => containsString(s))
    val test = one zip two zip three
    test

  println(lines)
  println(lines.head)
  println(checkString(lines))