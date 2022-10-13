package aoc2015

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Day03 extends App:
  val lines: String =
    Source
      .fromResource("aoc2015/Day03")
      .getLines
      .mkString

  def generateDirections(directions: String): Int =
    val even: String = directions.zipWithIndex.foldLeft("") {
      case (acc, (value, index)) if index % 2 == 0 => acc + value.toString
      case (acc, (value, index))                   => acc
    }
    val uneven: String = directions.zipWithIndex.foldLeft("") {
      case (acc, (value, index)) if index % 2 != 0 => acc + value.toString
      case (acc, (value, index))                   => acc
    }
    val Santa: List[(Int,Int)] = location(0,0,even)
    val RoboSanta: List[(Int,Int)] = location(0,0,uneven)
    val coordinates: List[(Int,Int)] = Santa ++ RoboSanta
    coordinates.distinct.size


  def location(currentx: Int, currenty: Int, directions: String, xAxis: List[Int] = List(0), yAxis: List[Int] = List(0)): List[(Int,Int)] =
    if directions.isEmpty then
      val coordinates = xAxis zip yAxis
      coordinates
    else
    directions.head match
      case '^' =>
        val newx = currentx + 0
        val newy = currenty + 1
        location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
      case 'v' =>
        val newx = currentx + 0
        val newy = currenty + -1
        location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
      case '<' =>
        val newx = currentx + -1
        val newy = currenty + 0
        location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
      case '>' =>
        val newx = currentx + 1
        val newy = currenty + 0
        location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
      case _ =>
        sys.error("Unknown command")

  val answer1: Int = location(0,0,lines).distinct.size
  println(answer1)
  val answer2: Int = generateDirections(lines)
  println(answer2)
