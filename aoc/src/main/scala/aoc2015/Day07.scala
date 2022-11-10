package aoc2015

import scala.io.Source

object Day07 extends App:

  case class Instr(signal: Int = 0, wire1: String = "", operator: String = "", wire2: String = "", value: Int = 0, outputWire: String = "")

  object Instr:

    def fromString(s: String): Instr =
      s match
        case s"$wire1 AND $wire2 -> $outputWire"      => Instr(wire1 = wire1, operator = "AND", wire2 = wire2, outputWire = outputWire)
        case s"$wire1 OR $wire2 -> $outputWire"       => Instr(wire1 = wire1, operator = "OR", wire2 = wire2, outputWire = outputWire)
        case s"$wire1 LSHIFT $value -> $outputWire"   => Instr(wire1 = wire1, operator = "LSHIFT", value = value.toInt, outputWire = outputWire)
        case s"$wire1 RSHIFT $value -> $outputWire"   => Instr(wire1 = wire1, operator = "RSHIFT", value = value.toInt, outputWire = outputWire)
        case s"NOT $wire1 -> $outputWire"             => Instr(wire1 = wire1, operator = "NOT", outputWire = outputWire)
        case s"$signal -> $outputWire"                => Instr(signal = signal.toInt, outputWire = outputWire)

  val instructions: List[Instr] =
    Source
      .fromResource("aoc2015/Test07")
      .getLines
      .map(Instr.fromString)
      .toList

  def initialize() =
    val x = 123
    val y = 456
    val d = x & y
    val e = x | y
    val f = x << 2
    val g = y >> 2
    val h = ~x & 0x0000FFFF
    val i = ~y & 0x0000FFFF
    s"d: $d, e: $e, f: $f, g: $g, h: $h, i: $i, x: $x, y: $y"

  println(initialize())
  println(instructions)
