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

  /** Exercise 6
    *
    * Implement map, flatMap, orElse and map2 for Either
    */
  sealed trait Either2[+E, +A] {
    def map[B](f: A => B): Either2[E, B] = this match {
      case Right2(a) => Right2(f(a))
      case Left2(err) => Left2(err)
    }

    def flatMap[EE >: E, B](f: A => Either2[EE, B]): Either2[EE, B] = this match {
      case Left2(err) => Left2(err)
      case Right2(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either2[EE, B]): Either2[EE, B] = this match {
      case Left2(_) => b
      case _ => this
    }

    def map2[EE >: E, B, C](b: Either2[EE, B])(f: (A, B) => C): Either2[EE, C] = (this, b) match {
      case (Right2(a), Right2(b)) => Right2(f(a, b))
      case (Left2(err), _) => Left2(err)
      case (_, Left2(err)) => Left2(err)
    }
  }

  case class Left2[+E](value: E) extends Either2[E, Nothing]
  case class Right2[+A](value: A) extends Either2[Nothing, A]

  /** Exercise 7
    *
    * Implement sequence and traverse for Either
    */
  def sequence3[A, B](as: List[Either2[A, B]]): Either2[A, List[B]] = {
    @annotation.tailrec def iter(as: List[Either2[A, B]], acc: List[B]): Either2[A, List[B]] = as match {
      case (left @ Left2(_)) :: _ => left
      case Right2(a) :: t => iter(t, a :: acc)
      case Nil => Right2(acc.reverse)
    }
    iter(as, Nil)
  }

  def traverse3[A, B, C](as: List[A])(f: A => Either2[B, C]): Either2[B, List[C]] = {
    @annotation.tailrec def iter(as: List[A], acc: List[C]): Either2[B, List[C]] = as match {
      case h :: t => f(h) match {
        case left @ Left2(_) => left
        case Right2(c) => iter(t, c :: acc)
      }
      case Nil => Right2(acc.reverse)
    }
    iter(as, Nil)
  }

  /** Exercise 8
    *
    * Implement a version of map2 that returns both errors if more than one error occurs
    *
    * Could a better data structure work for this? If so, create it and implement orElse, traverse, and sequence for it
    */
  def map2b[A, B, C, D](e1: Either2[A, B], e2: Either2[A, C])(f: (B, C) => D): Either2[List[A], D] = (e1, e2) match {
    case (Right2(b), Right2(c)) => Right2(f(b, c))
    case _ => Left2(List(e1, e2) collect { case Left2(error) => error })
  }

  sealed trait Validation[+E, +A] {
    def isSuccess = this match {
      case Success(_) => true
      case _ => false
    }

    def isFailure = !isSuccess

    def orElse[EE >: E, AA >: A](other: Validation[EE, AA]): Validation[EE, AA] = {
      if (isSuccess) this else (this, other) match {
        case (ErrorTrail(errors), ErrorTrail(moreErrors)) => ErrorTrail(errors ++ moreErrors)
        case (_, success @ Success(_)) => success
      }
    }


  }

  final case class ErrorTrail[E](errors: List[E]) extends Validation[E, Nothing]
  final case class Success[A](value: A) extends Validation[Nothing, A]
}
