package com.github.robertberry.fpis

object Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /** Exercise 1
    *
    * Write a function to generate a positive integer
    */
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = rng.nextInt
    ((n / 2) - (Int.MinValue / 2), newRng)
  }

  /** Exercise 2
    *
    * Write a function to generate a double between -1 and 1
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, newRng) = rng.nextInt

    val double = (((n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue)) * 2) - 1
    (double, newRng)
  }

  /** Exercise 3
    *
    * Write a function to generate an (Int, Double), a (Double, Int), and a (Double, Double, Double)
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)
  }

  def doubleDoubleDouble(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)
  }

  /** Exercise 4
    *
    * Write a function to generate a list of random integers
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec def iter(n: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      val (ns, rng) = acc

      if (n == 0) (ns.reverse, rng) else {
        val (i, nextRng) = rng.nextInt
        iter(n - 1, (i :: ns, nextRng))
      }
    }

    iter(count, (Nil, rng))
  }

  type Rand[+A] = RNG => (A, RNG)

  object Rand {
    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    /** Exercise 5
      *
      * Use map to reimplement double in a more elegant way
      */
    val double: Rand[Double] = map(int) { n =>
      (((n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue)) * 2) - 1
    }

    /** Exercise 6
      *
      * Write the implementation of map2, which takes two Rands and a function for combining their results, then returns
      * a new Rand that generates this combination
      */
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }

    /** Exercise 7
      *
      * Implement sequence, to combine an arbitrary-sized list of Rands into a single Rand
      */
    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
      rs match {
        case Nil => unit(Nil)
        case h :: t => map2(h, sequence(t))(_ :: _)
      }

    def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
  }
}
