package aoc2015

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Day03 extends App:
  val lines: String =
    Source
      .fromFile("aoc/src/resources/aoc2015/Day03")
      .getLines
      .mkString

  def generateDirections(): Int =
    var evenDirections: String = ""
    var unevenDirections: String = ""
    lines.zipWithIndex.foreach {
      case (x, i) =>
        if (i % 2 == 0)
          evenDirections = evenDirections.concat(x.toString)
        else
          unevenDirections = unevenDirections.concat(x.toString)
    }
    val even: List[(Int,Int)] = location(0,0,evenDirections)
    val oneven: List[(Int,Int)] = location(0,0,unevenDirections)
    val coo: List[(Int,Int)] = even ++ oneven
    coo.distinct.size


  def location(currentx: Int, currenty: Int, directions: String, xAxis: List[Int] = List(0), yAxis: List[Int] = List(0)): List[(Int,Int)] =
    if (directions.isEmpty) {
      val coordinates = xAxis zip yAxis
      coordinates
    }
    else if (directions.head.toString == "^") {
      val newx = currentx + 0
      val newy = currenty + 1
      location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
    }
    else if (directions.head.toString == "v") {
      val newx = currentx + 0
      val newy = currenty + -1
      location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
    }
    else if (directions.head.toString == "<") {
      val newx = currentx + -1
      val newy = currenty + 0
      location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
    }
    else if (directions.head.toString == ">") {
      val newx = currentx + 1
      val newy = currenty + 0
      location(newx, newy, directions.tail, xAxis :+ newx, yAxis :+ newy)
    }
    else {
      sys.error("Unknown command")
    }

  println(location(0,0,lines).distinct.size)
  println(generateDirections())

