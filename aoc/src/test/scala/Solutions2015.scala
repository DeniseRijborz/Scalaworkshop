import aoc2015.Day03
import org.scalatest.funsuite.AnyFunSuite

class Solutions2015 extends AnyFunSuite:

  test("Day03") {
    assertResult( 2565)(actual = Day03.answer1)
    assertResult( 2639)(actual = Day03.answer2)
  }