package com.github.robertberry.fpis

object Chapter3 {
  /** Exercise 2
    *
    * Implement tail, for removing the first element of a list
    */
  def tail[A](as: List[A]) = as match {
    case h :: t => t
    case Nil => ???
  }
  /** Here we throw an error on an empty list. Another option would be to return a wrapped type (using Option). */

  /** Exercise 3
    *
    * Using the same idea, implement setHead for replacing the first element of a list with a different value
    */
  def setHead[A](h: A, t: List[A]) = h :: tail(t)

  /** Exercise 4
    *
    * Generalize tail to the function drop
    */
  def drop[A](as: List[A], n: Int): List[A] = n match {
    case 0 => as
    case i if i < 0 => ???
    case _ => drop(tail(as), n - 1)
  }

  /** Exercise 5
    *
    * Implement dropWhile, which removes elements from the List prefix so long as they match the predicate
    */
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case h :: t if f(h) => dropWhile(t, f)
    case _ => as
  }

  /** Exercise 6
    *
    * Implement init, which returns a List of all but the last element
    */
  def init[A](as: List[A]): List[A] = as match {
    case a :: Nil => Nil
    case h :: t => h :: init(t)
    case Nil => ???
  }
  /** It can't be implemented in constant time as we have to traverse to near the end of the list, which is in O(n).
    * We also we have to create a new list, but this isn't as important in terms of running time.
    */

  /** Exercise 9
    *
    * Compute the length of a list using foldRight
    */
  def lengthRight[A](as: List[A]): Int = as.foldRight(0)((_, n) => n + 1)

  /** Exercise 10
    *
    * Implement foldLeft
    */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def iter(as: List[A], acc: B): B = as match {
      case Nil => acc
      case h :: t => iter(t, f(acc, h))
    }
    iter(as, z)
  }

  /** Exercise 11
    *
    * Implement sum, product and length using foldLeft
    */
  def sum(as: List[Int]) = foldLeft(as, 0)(_ + _)

  def product(as: List[Int]) = foldLeft(as, 1)(_ * _)

  def lengthLeft(as: List[Any]) = foldLeft(as, 0)((n, _) => n + 1)

  /** Exercise 12
    *
    * Implement reverse
    */
  def reverse[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def iter(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case h :: t => iter(t, h :: acc)
    }
    iter(as, Nil)
  }

  /** Exercise 13
    *
    * Define foldLeft in terms of foldRight
    * Define foldRight in terms of foldLeft
    */
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as.foldRight(identity[B] _) { case (a, b) => (acc: B) => b(f(acc, a)) } (z)
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as.foldLeft(identity[B] _) { case (b, a) => (acc: B) => b(f(a, acc)) } (z)
  }

  /** Exercise 14
    *
    * Define append in terms of foldRight
    */
  def append[A](xs: List[A], ys: List[A]): List[A] = xs.foldRight(ys)(_ :: _)

  /** Exercise 15
    *
    * Write a function that concatenates a list of lists. It should be linear in the total length of all the lists.
    */
  def flatten[A](xs: List[List[A]]): List[A] = xs.foldRight(List.empty[A])(append)
  /** To be so, it has to use foldRight here - otherwise it would be re-appending the accumulator each time */
}
