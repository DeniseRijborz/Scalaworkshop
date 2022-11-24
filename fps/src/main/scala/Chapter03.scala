/*
Chapter 3 Functional data structures
Pure function: doesn't change data or perform other side effects
Functional data structure: operated using only pure functions, therefore immutable
Singly linked list: collection of nodes, each node holds a value and a link to the next node
List(1,2,3) = Cons(1,Cons(2,Cons(3,Nil)))
Trait: abstract interface that may optionally contain implementations of some methods
Sealed trait: all implementations of the trait must be declared in this file
Pattern matching: examines and extract subexpressions, pattern => result
 */

// Exercise 3.1
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int =
    ints match
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)


  def product(ds: List[Double]): Double =
    ds match
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val x = List(1,2,3,4,5) match
  case Cons(x, Cons(2, Cons(4,_))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h,t) => h + List.sum(t)
  case _ => 101


// Exercise 3.2
def tail[A](l: List[A]): List[A] =
  l match
    case Cons(_,t) => t
    case Nil => sys.error("Empty list")


// Exercise 3.3
def setHead[A](l: List[A], x: A): List[A] =
  l match
    case Cons(_,t) => Cons(x,t)
    case Nil => sys.error("Empty list")


// Exercise 3.4
def drop[A](l: List[A], n: Int): List[A] =

