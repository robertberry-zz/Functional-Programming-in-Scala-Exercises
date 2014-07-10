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

  /*
  import Chapter11.Monad

  implicit class MonadExtensions1[F[_]](F: Monad[F]) {
    def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      override def flatMap[A, B](ma: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] = {
        F.flatMap(ma) { ga =>
          G.flatMap(ga) { a =>
            f(a)

            ???
          }
        }
      }
    }
  }
  */

  /** Exercise 12
    *
    * On the Applicative trait, implement sequence over a Map rather than a List
    */
  implicit class ApplicativeExtensions6[F[_]](F: Applicative[F]) {
    def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ofa.foldLeft(F.unit(Map.empty[K, V])) {
      case (facc, (k, fv)) =>
        F.map2(facc, fv) { (acc, v) =>
          acc + (k -> v)
        }
    }
  }

  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(identity)
  }

  /** Exercise 13
    *
    * Write Traverse instances for List, Option, Map and Tree
    */
  val listTraverse = new Traverse[List] {
    override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] =
      implicitly[Applicative[G]].sequence(fa.map(f))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
      val F = implicitly[Applicative[G]]
      fa.map(a => F.map(f(a))(Option.apply[B])) getOrElse F.unit(Option.empty[B])
    }
  }

  def mapTraverse[K] = new Traverse[({type h[V] = Map[K, V]})#h] {
    override def traverse[G[_]: Applicative, A, B](fa: Map[K, A])(f: (A) => G[B]): G[Map[K, B]] = {
      val F = implicitly[Applicative[G]]

      fa.foldLeft(F.unit(Map.empty[K, B])) { case (facc, (k, v)) =>
        F.map2(facc, f(v)) { (acc, v) =>
          acc + (k -> v)
        }
      }
    }
  }

  import com.github.robertberry.fpis.Chapter3.{Tree, Leaf, Branch, foldT}

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: (A) => G[B]): G[Tree[B]] = {
      val F = implicitly[Applicative[G]]

      foldT[A, G[Tree[B]]](fa)(a => F.map(f(a))(Leaf.apply)) { (fb1, fb2) =>
        F.map2(fb1, fb2) { (b1, b2) =>
          Branch(b1, b2)
        }
      }
    }
  }

  /** Exercise 14
    *
    * Implement map in terms of traverse as a method on Traverse[F].
    */
  type Id[A] = A

  val identityApplicative = new Applicative[Id] {
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)

    override def unit[A](a: => A): Id[A] = a
  }

  implicit class TraverseExtensions1[F[_]](traverse: Traverse[F]) {
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      traverse.traverse[Id, A, B](fa)(f)
  }

  
}
