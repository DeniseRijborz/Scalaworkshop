// Exercise 2.1
def fib(n: Int): Int =
  if n <= 1 then n
  else fib(n - 1) + fib(n - 2)

def fibRecursive(n: Int): Int =
  def go(current: Int, x0: Int, x1: Int): Int =
    if current == n then x1
    else go(current + 1, x1, x0 + x1)
  if n <= 1 then n
  else go(1, 0, 1)


// Exercise 2.2
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =
  def loop(n: Int): Boolean =
    if n == as.length-1 then true
    else if !ordered(as(n), as(n + 1)) then false
    else loop(n + 1)
  loop(0)

def ordered(n: Int, m: Int) =
  n <= m

// Exercise 2.3
def curry[A,B,C](f: (A,B) => C): A => (B => C) =
  (a: A) => ((b: B) => f(a,b))

// Exercise 2.4
def uncurry[A,B,C](f: A => B => C): (A,B) => C =
  (a: A, b: B) => f(a)(b)

// Exercise 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

