package com.github.robertberry.fpis

import com.github.robertberry.fpis.Chapter11.Functor

object Chapter12 {
  trait Applicative[F[_]] extends Functor[F] {
    // primitive combinators
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]
    // derived combinators
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }

  /** Exercise 1
    *
    * Transplant the implementations of as many combinators as you can from Monad to Applicative, using only map2
    * and unit, or methods implemented in terms of them.
    */
  implicit class ApplicativeExtensions1[F[_]](applicative: Applicative[F]) {
    def sequence[A](fas: List[F[A]]): F[List[A]] = applicative.traverse(fas)(identity)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A,B)] =
      applicative.map2(fa, fb)(_ -> _)
  }

  /** Exercise 2
    *
    * Implement apply, then map and map2 in terms of it.
    */
  implicit class ApplicativeExtensions2[F[_]](applicative: Applicative[F]) {
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      applicative.map2(fab, fa)(_(_))

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      apply(applicative.unit[A => B](f))(fa)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(applicative.unit[A => B => C]({ a: A => { b: B => f(a, b) } }))(fa))(fb)
  }

  /** Exercise 3
    *
    * Implement map3 and map4 using only unit, apply, and the curried method available on functions
    */
  implicit class ApplicativeExtensions3[F[_]](ap: Applicative[F]) {
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      ap.apply(ap.apply(ap.apply(ap.unit[A => B => C => D](f.curried))(fa))(fb))(fc)
    }

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      ap.apply(ap.apply(ap.apply(ap.apply(ap.unit[A => B => C => D => E](f.curried))(fa))(fb))(fc))(fd)
    }
  }

  /** Exercise 4
    *
    * What is the meaning of streamApplicative.sequence?
    *
    * Given a list of streams, returns a stream of lists of elements from each stream (so the first thing emitted would
    * be a list of the heads of each stream, then the next would be a list of the second items from each stream, etc.),
    * until one stream ends.
    */

  /** Ignoring exercise 5 as I've done it previously (implement a Monad for Either) */

  /** Exercise 6
    *
    * Write an Applicative instance for Validation that accumulates errors in Failure.
    */
  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E] = new Applicative[({type f[A] = Validation[E, A]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (f @ Failure(_, _), Success(_)) => f
        case (Success(_), f @ Failure(_, _)) => f
        case (Failure(leftH, leftT), Failure(rightH, rightT)) =>
          Failure(leftH, (leftT :+ rightH) ++ rightT)
      }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

  /** Exercise 7
    *
    * Prove that all monads are applicative functors by showing that if the monad laws hold, the Monad implementations
    * of map and map2 satisfy the applicative laws.
    *
    * -- TODO
    */

  /** Exercise 8
    *
    * Implement product
    */
  implicit class ApplicativeExtensions4[F[_]](F: Applicative[F]) {
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def map2[A, B, C](fga: (F[A], G[A]), fgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
          val (fa, ga) = fga
          val (fb, gb) = fgb

          (F.map2(fa, fb)(f), G.map2(ga, gb)(f))
        }

        override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))
      }
  }

  /** Exercise 9
    *
    * Implement compose
    */
  implicit class ApplicativeExtensions5[F[_]](F: Applicative[F]) {
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        F.map2(fa, fb) { (ga, gb) =>
          G.map2(ga, gb)(f)
        }

      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    }
  }

  /** Exercise 10
    *
    * Prove that this composite applicative functor meets the applicative laws. This is an extremely challenging exercise.
    *
    * --- TODO
    */

  /** Exercise 11
    *
    * Try to implement compose on Monad. It's not possible but it's constructive to figure out why.
    */
  import Chapter11.Monad

  implicit class MonadExtensions1[F[_]](M: Monad[F]) {
    
  }
}
