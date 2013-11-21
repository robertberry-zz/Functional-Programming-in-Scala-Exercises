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
