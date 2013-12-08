package com.github.robertberry.fpis

import java.util.concurrent.{Callable, TimeUnit, ExecutorService, Future}

object Chapter7 {
  /** Exercise 1
    *
    * Par.map2 is a new higher-order function for combining the result of two parallel computations. What is its
    * signature?
    *
    * Answer: map2[A, B, C](Par[A], Par[B])(f: (=> A, => B) => C): Par[C]
    */

  object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    case class UnitFuture[A](get: A) extends Future[A] {
      def cancel(mayInterruptIfRunning: Boolean): Boolean = false

      def isCancelled: Boolean = false

      def isDone: Boolean = true

      def get(timeout: Long, unit: TimeUnit): A = get
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { (es: ExecutorService) =>
      val af = a(es)
      val bf = b(es)

      UnitFuture(f(af.get, bf.get))
    }

    def fork[A](a: Par[A]): Par[A] = es => es.submit(new Callable[A] {
      def call = a(es).get
    })

    def async[A](a: => A): Par[A] = fork(unit(a))

    def run[A](es: ExecutorService)(a: Par[A]): A = a(es).get

    /** Exercise 3
      *
      * Fix the implementation of map2 so as to respect timeouts
      */
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { (es: ExecutorService) =>
      val af = a(es)
      val bf = b(es)

      new Future[C] {
        def cancel(mayInterruptIfRunning: Boolean): Boolean = true

        def isCancelled: Boolean = af.isCancelled || bf.isCancelled

        def isDone: Boolean = af.isDone && bf.isDone

        def get(): C = f(af.get, bf.get)

        def get(timeout: Long, unit: TimeUnit): C = {
          val started = System.currentTimeMillis

          val a = af.get(timeout, unit)
          val elapsed = System.currentTimeMillis - started
          val remaining = unit.toMillis(timeout) - elapsed
          val b = bf.get(remaining, unit)

          f(a, b)
        }
      }
    }

    /** Exercise 4
      *
      * Write a function to convert any A => B to an asynchronous computation
      */
    def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

    /** Exercise 5
      *
      * Implement sequence, which converts a List[Par[A]] to a Par[List[A]]]
      */
    def sequence[A](as: List[Par[A]]): Par[List[A]] = as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, sequence(t))(_ :: _)
    }

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork { sequence(as.map(asyncF(f))) }

    /** Exercise 6
      *
      * Implement parFilter, which filters elements of a list in parallel
      */
    /*def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
        TODO it is late ...
    } */
  }
}
