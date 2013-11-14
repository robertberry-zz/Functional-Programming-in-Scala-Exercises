package com.github.robertberry.fpis

object Chapter2 {
  /** Exercise 1
    *
    * Write a recursive function to get the nth fibonacci number. Your definition should use a local tail-recursive
    * function.
    */
  def fib(n: Int) = {
    @annotation.tailrec
    def iter(n: Int, prev: Int, prev2: Int): Int = {
      if (n == 0) prev
      else iter(n - 1, prev + prev2, prev)
    }
    n match {
      case 1 => 1
      case 2 => 1
      case n => iter(n - 2, 1, 1)
    }
  }

  /** Exercise 2
    *
    * Check isSorted, which checks whether an Array[A] is sorted according to a given comparison function.
    */
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def iter(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else !gt(as(i), as(i + 1)) && iter(i + 1)
    }
    iter(0)
  }

  /** Exercise 3
    *
    * Let's look at another example, currying, which converts a function f of two arguments into a function of one
    * argument that partially applies f
    */
  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  /** Exercise 4
    *
    * Implement uncurry, which reverses the transformation of curry.
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /** Exercise 5
    *
    * Implement the higher-order function that composes two functions.
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
