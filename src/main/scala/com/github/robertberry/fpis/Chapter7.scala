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
      *
      * NB: This would obviously be really inefficient ...
      */
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = l match {
      case Nil => unit(Nil)
      case h :: t => map2(unit(f(h)), parFilter(t)(f)) { (matches, rest) =>
         if (matches) h :: rest else rest
      }
    }

    // TODO below two functions are ugly and stupid, fix them
    def reduce[A](as: IndexedSeq[A], zero: A)(f: (A, A) => A): Par[A] = {
      def iter(as: IndexedSeq[A]): Par[A] = {
        if (as.isEmpty) unit(zero)
        else {
          val left = fork(iter(as.take(as.length / 2)))
          val right = fork(iter(as.drop(as.length / 2)))

          map2(left, right)(f)
        }
      }

      iter(as)
    }

    def parallelMax[A](as: IndexedSeq[A])(max: (A, A) => A): Par[A] = reduce(as, as.head)(max)

    /** Implement map3, map4, and map5 in terms of map2 */
    def map3[A, B, C, D](fa: Par[A], fb: Par[B], fc: Par[C])(f: (A, B, C) => D): Par[D] = {
      map2(map2(fa, fb)((a, b) => (c: C) => f(a, b, c)), fc)(_(_))
    }

    def map4[A, B, C, D, E](fa: Par[A], fb: Par[B], fc: Par[C], fd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
      map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => f(a, b, c, d)), fc)(_(_)), fd)(_(_))
    }

    def map5[A, B, C, D, E, F](fa: Par[A], fb: Par[B], fc: Par[C], fd: Par[D], fe: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
      map2(map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => (e: E) => f(a, b, c, d, e)), fc)(_(_)), fd)(_(_)), fe)(_(_))
    }

    // It's times like this that I wish I were using Haskell ...

    /** Exercise 7
      *
      * Given map(y)(id) == y it is a free theorem that map(map(y)(g))(f) == map(y)(f compose g). Prove it.
      *
      * My knowledge of mathematical proof techniques is poor so this might be wrong.
      *
      * map(y)(f) == map(y)(f)                      -- trivially true
      * map(map(y)(id))(f) == map(y)(f)             -- as map(y)(id) == y
      * map(map(y)(id))(f) == map(y)(f compose id)  -- as f compose id == f
      * map(map(y)(g))(f) == map(y)(f compose g)    -- as id can be swapped with an arbitrary g because of the free
      *                                                theorem
      */

    /** Exercise 8
      *
      * Show why fork(x) == x does not hold for all ExecutorServices
      *
      * The implementation of fork queues the computation to be executed in the threadpool then blocks on that Future.
      * This is a problem, however, as the singleThreadedExecutorPool only has one thread, so this will result in a
      * non-terminating computation due to deadlock. (i.e., the single thread in the pool is blocked waiting for a
      * Callable that will never be dequeued, due to the thread being blocked)
      */

    /** Exercise 9
      *
      * Can you show that any sized threadpool can be made to deadlock given this implementation of fork?
      */
    def deadlock[A](threadPoolSize: Int, a: A): Par[A] = {
      if (threadPoolSize <= 1) {
        async(a)
      } else {
        fork(deadlock(threadPoolSize - 1, a))
      }
    }


  }
}

