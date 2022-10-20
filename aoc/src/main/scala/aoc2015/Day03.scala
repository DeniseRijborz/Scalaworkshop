package aoc2015

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Day03 extends App:
  val lines: String =
    Source
      .fromResource("aoc2015/Day03")
      .getLines
      .mkString

  case class Loc(x: Int, y: Int)

  def location(current: Loc, directions: String, locations: List[Loc]): List[Loc] =
    if directions.isEmpty then
      locations
    else
    directions.head match
      case '^' =>
        val newLoc = Loc(current.x, current.y + 1)
        location(newLoc, directions.tail, locations :+ newLoc)
      case 'v' =>
        val newLoc = Loc(current.x, current.y - 1)
        location(newLoc, directions.tail, locations :+ newLoc)
      case '<' =>
        val newLoc = Loc(current.x - 1, current.y)
        location(newLoc, directions.tail, locations :+ newLoc)
      case '>' =>
        val newLoc = Loc(current.x + 1, current.y)
        location(newLoc, directions.tail, locations :+ newLoc)
      case _ =>
        sys.error("Unknown command")

  def generateDirections(directions: String): Int =
    val Santa: String = directions.zipWithIndex.foldLeft("") {
      case (acc, (value, index)) if index % 2 == 0 => acc + value.toString
      case (acc, (value, index)) => acc
    }
    val RoboSanta: String = directions.zipWithIndex.foldLeft("") {
      case (acc, (value, index)) if index % 2 != 0 => acc + value.toString
      case (acc, (value, index)) => acc
    }
    val totalLocations: List[Loc] = location(Loc(0, 0), Santa, List()) ++ location(Loc(0, 0), RoboSanta, List())
    totalLocations.distinct.size

  val answer1: Int = location(Loc(0,0),lines,List()).distinct.size
  println(answer1)
  val answer2: Int = generateDirections(lines)
  println(answer2)
