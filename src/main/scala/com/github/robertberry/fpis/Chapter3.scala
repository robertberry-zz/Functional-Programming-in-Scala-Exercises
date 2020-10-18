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
  def reverse[A](as: List[A]): List[A] = foldLeft[A, List[A]](as, Nil)((acc, s) => s::acc)

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

  /** Exercise 16
    *
    * Write a function that transfroms a list of integers by adding 1 to each element
    */
  def add1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case h :: t => h + 1 :: add1(t)
  }

  /** Exercise 17
    *
    * Write a function that turns a list of doubles into a list of strings
    */
  def stringify(xs: List[Double]): List[String] = xs match {
    case Nil => Nil
    case h :: t => h.toString :: stringify(t)
  }

  /** Exercise 18
    *
    * Write a function map that generalizes modifying each element in a list
    */
  def mapF[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case h :: t => f(h) :: mapF(t)(f)
  }

  /** Exercise 19
    *
    * Write a function filter that removes elements from a list unless they satisfy the given predicate
    */
  def filterF[A](as: List[A])(f: A => Boolean): List[A] =
    as.foldRight(List.empty[A])((a, acc) => if (f(a)) a :: acc else acc)

  /** Exercise 20
    *
    * Implement flatMap, that works like map except that the function will return a list, and that the resultant values
    * should be flattened into a single list
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case h :: t => f(h) ++ flatMap(t)(f)
  }

  /** Exercise 21
    *
    * Implement filter in terms of flatMap
    */
  def filterF2[A, B](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  /** Exercise 22
    *
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements
    */
  def sum2(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x :: xs, y :: ys) => (x + y) :: sum2(xs, ys)
  }

  /**
   * Exercise 23
   *
   * Generalize the above function so that it's not specific to integers or addition
   */
  def map2[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (a :: as, b :: bs) => f(a, b) :: map2(as, bs)(f)
  }

  /** Exercise 24
    *
    * Implement hasSubsequence, checking whether a list contains another list
    */
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    l.length >= sub.length && (l.take(sub.length) == sub || hasSubsequence(l.tail, sub))

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  /** Exercise 25
    *
    * Write a function size that counts the number of nodes (leaves and branches) in a tree
    */
  def sizeT(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => sizeT(left) + sizeT(right) + 1
  }

  /** Exercise 26
    *
    * Write a function maximum that returns the maximum element of a Tree of Int
    */
  def maximumT(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(left, right) => maximumT(left) max maximumT(right)
  }

  /** Exercise 27
    *
    * Write a function that computes the maximum path length from the root to any leaf
    */
  def depthT(tree: Tree[_]): Int = tree match {
    case Leaf(n) => 1
    case Branch(left, right) => 1 + (depthT(left) max depthT(right))
  }

  /** Exercise 28
    *
    * Write a map for Tree, analogous to that for List
    */
  def mapT[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(mapT(left)(f), mapT(right)(f))
  }

  /** Exercise 29
    *
    * Write a fold for Tree, analogous to that for List
    *
    * Rewrite size, maximum, depth, and map in terms of fold
    */
  def foldT[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(foldT(left)(f)(g), foldT(right)(f)(g))
  }

  def fSize(tree: Tree[_]): Int = foldT(tree)(_ => 1)((left, right) => left + right + 1)
  def fMax(tree: Tree[Int]): Int = foldT(tree)(identity)(_ max _)
  def fDepth(tree: Tree[_]): Int = foldT(tree)(_ => 1)((x, y) => (x max y) + 1)
  def fMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = foldT(tree)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
}
