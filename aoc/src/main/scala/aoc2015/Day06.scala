package aoc2015

import scala.io.Source
import scala.util.matching.Regex

object Day06 extends App:

  val lines: List[String] =
    Source
      .fromResource("aoc2015/Day06")
      .getLines
      .toList

  case class coordinate(x: Int, y: Int)

  def grid(c: coordinate, d: coordinate): IndexedSeq[List[Int]] =
    for {
      w <- c.x to d.x
      z <- c.y to d.y
    } yield List(w, z, 0)
//  val init: List[List[Int]] =
//    List.tabulate(3,3)((_,_) => 0)
//  println(init)
  val gridCoordinates = grid(coordinate(0,0),coordinate(999,999))

  def turnonLights(instructions: List[String], lights: IndexedSeq[List[Int]]): IndexedSeq[List[Int]] =
    if instructions.isEmpty then
      lights
    else
      val numbers = ("""\d+""".r findAllIn instructions.head).toList.map(_.toInt)
      instructions.head match
        case x if x.contains("turn on") =>
          val turnon = grid(coordinate(numbers(0),numbers(1)),coordinate(numbers(2),numbers(3)))
          turnonLights(
            instructions.tail,
            gridCoordinates
              .map(x =>
                if turnon.map(t => t.slice(0,2)).contains(x.slice(0,2)) then x.dropRight(1):+1
                else x))
        case x if x.contains("turn off") =>
          val turnoff = grid(coordinate(numbers(0),numbers(1)),coordinate(numbers(2),numbers(3)))
          turnonLights(
            instructions.tail,
            gridCoordinates
              .map(x =>
                if turnoff.map(t => t.slice(0, 2)).contains(x.slice(0, 2)) then x.dropRight(1) :+ 0
                else x))
        case x if x.contains("toggle") =>
          val toggle = grid(coordinate(numbers(0),numbers(1)),coordinate(numbers(2),numbers(3)))
          turnonLights(
            instructions.tail,
            gridCoordinates
              .map(x =>
                if toggle.map(t => t.slice(0, 2)).contains(x.slice(0, 2)) then
                  if x.last == 1 then x.dropRight(1) :+ 0
                  else x.dropRight(1) :+ 1
                else x))
        case _ => sys.error("Unknown command")

  println(turnonLights(lines, gridCoordinates).map(x => x.last).sum)
