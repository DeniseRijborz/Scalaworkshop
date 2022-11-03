object Chapter02 extends App:

  // Exercise 2.1
  def fib(n: Int): Int =
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)

  println(fib(9))
  //