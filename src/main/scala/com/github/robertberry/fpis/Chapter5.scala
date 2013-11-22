package com.github.robertberry.fpis

object Chapter5 {
  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](h: => A, t: => Stream[A]): Stream[A] = new Cons[A] {
      lazy val head = h
      lazy val tail = t
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

  sealed trait Stream[+A] {
    def uncons: Option[Cons[A]]

    def isEmpty: Boolean = uncons.isEmpty

    /** Exercise 1
      *
      * Write toList, which forces evaluation of a Stream and converts it to a List
      */
    def toList: List[A] = uncons.map(cell => cell.head :: cell.tail.toList).getOrElse(Nil)

    /** Exercise 2
      *
      * Write take for returning the first n elements of a Stream
      */
    def take(n: Int): Stream[A] = n match {
      case i if i <= 0 => Empty
      case i => this match {
        case Empty => Empty
        case cons: Cons[A] => Stream.cons(cons.head, cons.tail.take(n - 1))
      }
    }

    /** Exercise 3
      *
      * Write takeWhile, for returning all starting elements of a Stream that match the given predicate
      */
    def takeWhile(predicate: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case cell: Cons[A] => if (!predicate(cell.head)) Empty else Stream.cons(cell.head, cell.tail.takeWhile(predicate))
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      uncons match {
        case Some(c) => f(c.head, c.tail.foldRight(z)(f))
        case None => z
      }
    }

    def exists(p: A => Boolean): Boolean = {
      foldRight(false)((a, b) => p(a) || b)
    }

    /** Exercise 4
      *
      * Implement forAll, which checks that all elements in a Stream match a given predicate. It should terminate
      * early as soon as it reaches a non-matching value
      */
    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)

      /**
       * Could also do !exists(complement(p)) with the same guarantee of early termination
       */
    }

    /** Exercise 5
      *
      * Use foldRight to implement takeWhile. This will construct a stream incrementally, only if the values are
      * demanded by some other expression
      */
    def takeWhile2(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)
    }

  }

  object Empty extends Stream[Nothing] {
    val uncons = None
  }

  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A

    def tail: Stream[A]

    val uncons = Some(this)
  }

  def toStream[A](xs: List[A]): Stream[A] = xs match {
    case Nil => Empty
    case h :: t => Stream.cons(h, toStream(t))
  }
}
