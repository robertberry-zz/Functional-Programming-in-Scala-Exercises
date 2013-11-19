package com.github.robertberry.fpis

object Chapter4 {
  /** Calling this Maybe, Just and Absent to avoid name conflicts ... */

  /** Exercise 1
    *
    * Implement map, flatMap, getOrElse, orElse and filter
    */
  sealed trait Maybe[+A] {
    def map[B](f: A => B): Maybe[B] = this match {
      case Absent => Absent
      case Just(a) => Just(f(a))
    }

    def flatMap[B](f: A => Maybe[B]) = this match {
      case Absent => Absent
      case Just(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Just(a) => a
      case _ => default
    }

    def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = this match {
      case Absent => ob
      case _ => this
    }

    def filter(f: A => Boolean) = this match {
      case Just(a) if f(a) => this
      case _ => Absent
    }

    def isDefined: Boolean = this match {
      case Absent => false
      case _ => true
    }

    def get: A
  }
  case class Just[A](get: A) extends Maybe[A]
  case object Absent extends Maybe[Nothing] {
    def get = throw new UnsupportedOperationException("get not defined on Absent")
  }

  /** Exercise 2
    *
    * Implement variance in terms of mean and flatMap
    */
  def mean(xs: Seq[Double]): Maybe[Double] =
    if (xs.isEmpty) Absent
    else Just(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Maybe[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /** Exercise 3
    *
    * Declare map2, which combines two Maybes using a binary function
    */
  def map2[A, B, C](a: Maybe[A], b: Maybe[B])(f: (A, B) => C): Maybe[C] = (a, b) match {
    case (Just(x), Just(y)) => Just(f(x, y))
    case _ => Absent
  }

  /** Exercise 4
    *
    * Declare a function sequence, that combines a list of Maybes into a Maybe of a list of what was inside the Justs.
    * If any Maybe is Absent, should be Absent
    */
  def sequence[A](as: List[Maybe[A]]): Maybe[List[A]] = as.foldRight(Just(List.empty[A]): Maybe[List[A]]) {
    case (a, maybeAcc) => maybeAcc.flatMap(acc => a.map(_ :: acc))
  }

  /** Exercise 5
    *
    * Implement traverse, which shortcut exits early if any value for f is Absent
    *
    * Reimplement sequence in terms of traverse
    */
  def traverse[A, B](as: List[A])(f: A => Maybe[B]): Maybe[List[B]] = as match {
    case Nil => Just(Nil)
    case h :: t => f(h).flatMap(fh => traverse(t)(f).map(fh :: _))
  }

  def sequence2[A](as: List[Maybe[A]]): Maybe[List[A]] = traverse(as)(identity)
}
