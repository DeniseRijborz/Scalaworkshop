/*
Chapter 3 Functional data structures
Pure function: doesn't change data or perform other side effects
Functional data structure: operated using only pure functions, therefore immutable
Singly linked list: collection of nodes, each node holds a value and a link to the next node
List(1,2,3) = Cons(1,Cons(2,Cons(3,Nil)))
Trait: abstract interface that may optionally contain implementations of some methods
Sealed trait: all implementations of the trait must be declared in this file
Pattern matching: examines and extract subexpressions, pattern => result
ADT Algebraic Data type: a data type defined by one or more data constructors, each of which may contain zero
or more arguments. The data type is the sum or union of its data constructors, and each data constructor
is the product of its arguments. List is an example.
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

// Exercise 3.13

// Exercise 3.14
// Implement append in terms of either foldLeft or foldRight
def append[A](l: List[A], l2: List[A]): List[A] =
  foldRight(l,l2)(Cons(_,_))

// Exercise 3.15
// Write a function that concatenates a list of lists into a single list.
def concat[A](l: List[List[A]]): List[A] =
  foldLeft(l, Nil: List[A])(append)

// Exercise 3.16
// Write a function that transforms a list of integers by adding 1 to each element.
def add1(l: List[Int]): List[Int] =
  foldRight(l, Nil: List[Int])((h,t)=>Cons(h + 1, t))

// Exercise 3.17
// Write a function that turns each value in a List[Double] into a String.
def doubleToString(l: List[Double]): List[String] =
  foldRight(l, Nil: List[String])((h,t)=> Cons(h.toString,t))

// Exercise 3.18
// Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
def map[A,B](as: List[A])(f: A => B): List[B] =
  foldRight(as, Nil: List[B])((h,t) => Cons(f(h),t))

// Exercise 3.19
// Write a function filter that removes elements from a list unless they satisfy a given predicate.
def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight(as, Nil: List[A])((h,t) => if f(h) then Cons(h,t) else t)

def removeOdds(A: Int): Boolean =
  A % 2 == 0
// filter(List(1,2,3,4))(removeOdds) = Cons(2,(Cons(4,Nil))

// Exercise 3.20
// Write a function flatMap that works like map except that the function given will return a list instead of a
// single result, and that list should be inserted into the final resulting list.
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  concat(map(as)(f))
// flatMap(List(1,2,3))(i => List(i,i)) = Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil))))))

// Exercise 3.21
// Use flatMap to implement filter.
def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
  flatMap(as)(a => if f(a) then List(a) else Nil)

// Exercise 3.22
// Write a function that accepts two lists and constructs a new list by adding corresponding elements.
def addElements(l: List[Int], r: List[Int]): List[Int] =
  (l,r) match
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h, t),Cons(h2, t2)) => Cons(h + h2, addElements(t, t2))

// Exercise 3.23
// Generalize the function so that it's not specific to integers or addition.
def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] =
  (l,r) match
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h, t),Cons(h2, t2)) => Cons(f(h, h2), zipWith(t, t2)(f))
// zipWith(List(1,2,3),List("a","b","c"))(_+_) = Cons(1a,Cons(2b,Cons(3c,Nil)))

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Exercise 3.25
// Write a function size that counts the number of nodes (leaves and branches) in a tree.
def size[A](t: Tree[A]): Int =
  t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
// size(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) = 5

// Exercise 3.26
// Write a function maximum that returns the maximum element in a Tree[Int].
def maximum(t: Tree[Int]): Int =
  t match
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
// maximum(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) = 3

// Exercise 3.27
// Write a function depth that returns the maximum path length from the root of a tree to any leaf
def depth[A](t: Tree[A]): Int =
  t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))

// Exercise 3.28
// Write a function map that modified each element in a tree with a given function.
def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
  t match
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
// map(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)))(_*2) = Branch(Branch(Leaf(2),Leaf(4)),Leaf(6))

// Exercise 3.29
// Generalize size, maximum, depth and map, writing a new function fold that abstracts over their similarities.
def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
  t match
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))

def sizeViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 1)(1 + _ + _)

def maximumViaFold(t: Tree[Int]): Int =
  fold(t)(a => a)(_ max _)

def depthViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

