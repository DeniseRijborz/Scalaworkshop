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
// the third case will be the first to match, x + y, the outcome will be 1 + 2 = 3

// Exercise 3.2
// Function tail for removing the first element of a List
def tail[A](l: List[A]): List[A] =
  l match
    case Cons(_,t) => t
    case Nil => sys.error("Empty list")
// tail(List(1,2,3)) -> Cons(2,Cons(3,Nil))

// Exercise 3.3
// Function setHead for replacing the first element of a List with a different value
def setHead[A](l: List[A], x: A): List[A] =
  l match
    case Cons(_,t) => Cons(x,t)
    case Nil => sys.error("Empty list")
// setHead(List(1,2,3),4) -> Cons(4,Cons(2,Cons(3,Nil)))

// Exercise 3.4
// Generalize tail to the function drop, which removes the first n elements from a list
def drop[A](l: List[A], n: Int): List[A] =
  if n == 0 then l
  else l match
    case Cons(_,t) => drop(t, n - 1)
    case Nil => sys.error("Empty list")
// drop(List(1,2,3,4),2) -> Cons(3,Cons(4,Nil))

// Exercise 3.5
// Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Nil => sys.error("Empty list")
    case _ => l
// dropWhile(List(1,2,3),(x:Int)=> x < 2) -> Cons(2,Cons(3,Nil))

// Exercise 3.6
// Implement function init that returns a list consisting of all but the last element of a List
def init[A](l: List[A]): List[A] =
  l match
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => sys.error("Empty List")
// init(List(1,2,3)) -> Cons(1,Cons(2,Nil))

// Group the arguments in dropWhile (curry), so we don't need to state the type of f, type info flows from left to right
// across argument groups
def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] =
  l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Nil => sys.error("Empty list")
    case _ => l
// dropWhileCurried(List(1,2,3))(x => x<2) -> Cons(2,Cons(3,Nil))

// Exercise 3.7
def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs, z)(f))

def sum2(ns: List[Int]) =
  foldRight(ns,0)((x,y) => x + y)

def product2(ns: List[Double]) =
  foldRight(ns,1.0)(_ * _)

// Exercise 3.8
// foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) -> Cons(1,Cons(2,Cons(3,Nil)))

// Exercise 3.9
// Compute the length of a list using foldRight
def length[A](as: List[A]): Int =
  foldRight(as,0)((_, acc) => acc + 1)

// Exercise 3.10
// foldRight not tail-recursive so will result in StackOverflowError, implement function foldLeft that is tail-recursive
def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
  as match
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)

// Exercise 3.11
// Write sum, product and a function to compute the length of a list using foldLeft
def sum3(ns: List[Int]) =
  foldLeft(ns,0)(_ + _)

def product3(ns: List[Double]) =
  foldLeft(ns,1.0)(_ * _)

def length2[A](as: List[A]): Int =
  foldLeft(as,0)((acc, _) => acc + 1)

// Exercise 3.12
// Write a function that returns the reverse of a list
def reverse[A](ns: List[A]): List[A] =
  foldLeft(ns,List[A]())((acc,x) => Cons(x, acc))
// reverse(List(1,2,3,4,5)) ->
// foldLeft(Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil))))), List[A]())((acc,x) => Cons(x,acc))
//
