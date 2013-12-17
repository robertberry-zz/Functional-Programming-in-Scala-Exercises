package com.github.robertberry.fpis

object Chapter8 {
  /** Exercise 1
    *
    * Try thinking of properties to specify the implementation of a sum: List[Int] => Int function
    *
    * - If List[Int] is split to h :: t, sum(h :: t) == h + sum(t)
    * - More generally, if xs is split into as ++ bs, sum(as) ++ sum(bs) == sum(xs)
    * - If x is the same value for all xs, x * len(xs) == sum(xs)
    * - sum(-sum(xs) :: xs) == 0
    */

  /** Exercise 2
    *
    * What are properties that specifies a function that finds the maximum of a List[Int]
    *
    * - max(xs) == max(reverse(xs))
    * - given xs == as ++ bs, max(List(max(as), max(bs))) == max(xs)
    * - If x is the same value for all xs, max(xs) == x
    * - If x is the same value for all xs, max((x - 1) :: xs) == x
    * - If x is the same value for all xs, max((x + 1) :: xs) == x + 1
    */

  /** Exercise 3
    *
    * Assuming the following definition of Prop, implement &&
    */
  object Ex3 {
    trait Prop { p1 =>
      def check: Boolean

      def &&(p2: Prop) = new Prop {
        def check: Boolean = p1.check && p2.check
      }
    }
  }

  import Chapter6.{RNG, State, Rand}

  object Gen {
    /** Exercise 4
      *
      * Implement choose
      */
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(Rand.map(Rand.positiveLessThan(stopExclusive - start))(_ + start)))

    /** Exercise 5
      *
      * Implement unit, boolean, and listOfN
      */
    def unit[A](a: A): Gen[A] = Gen(State(Rand.unit(a)))

    val boolean: Gen[Boolean] = Gen(State(Rand.map(Rand.positiveLessThan(2))(_ == 1)))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State(Rand.sequence(List.fill(n)(g.sample.run))))

    def sameParity(from: Int, to: Int): Gen[(Int, Int)] = choose(from, to) flatMap { a: Int =>
      val lower = if (a >= 0) Math.max(0, from) else from
      val higher = if (a < 0) Math.min(0, to) else to
      choose(lower, higher) map { b: Int => (a, b) }
    }

    def listOfN2[A](size: Gen[Int], g: Gen[A]): Gen[List[A]] = size flatMap { n: Int => listOfN(n, g) }

    /** Exercise 7
      *
      * Implement union, for combining two generators of the same type into one, by pulling values from each generator
      * with equal likelihood
      */
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap { if (_: Boolean) g1 else g2 }

    /** Exercise 8
      *
      * Implement weighted
      *
      * NB: You were supposed to use doubles, but I'm too lazy to implement a Gen primitive for double, so using ints.
      *     The code wouldn't really change, anyway.
      *
      *     It's expected that both of the weights are positive integers.
      */
    def weighted[A](g1: (Gen[A], Int), g2: (Gen[A], Int)): Gen[A] = choose(0, g1._2 + g2._2) flatMap { w: Int =>
      if (w < g1._2) g1._1 else g2._1
    }
  }

  /** Exercise 6
    *
    * Implement flatMap, then sameParity and a more dynamic listOfN in terms of it
    */
  case class Gen[+A](sample: State[RNG, A]) {
    def flatMap[AA >: A, B](f: AA => Gen[B]): Gen[B] = {
      Gen(State(Rand.flatMap(sample.run)(f.andThen(_.sample.run))))
    }

    def map[AA >: A, B](f: AA => B): Gen[B] = flatMap[AA, B](f.andThen(Gen.unit[B]))

    /** Exercise 10
      *
      * Implement helper functions for converting Gen to SGen
      */
    def unsized: SGen[A] = SGen(const(this))
  }

  object Ex9 {
    object Prop {
      type FailedCase = String
      type SuccessCount = Int
      type TestCases = Int
      type Result = Option[(FailedCase, SuccessCount)]
    }

    /** Exercise 9
      *
      * Implement && and || for manipulating Prop values
      */
    case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {
      def &&(p2: Prop) = Prop { (testCases, rng) =>
        val r1 = run(testCases, rng)
        r1 orElse p2.run(testCases, rng)
      }

      def ||(p2: Prop) = Prop { (testCases, rng) =>
        val r1 = run(testCases, rng)

        r1 flatMap { _ => p2.run(testCases, rng) }
      }
    }
  }

  /** Exercise 11
    *
    * Implement the same convenience functions on SGen as Gen
    */
  case class SGen[+A](forSize: Int => Gen[A]) {
    def flatMap[AA >: A, B](f: AA => SGen[B]): SGen[B] = SGen { i: Int =>
      forSize(i) flatMap { gen: A => f(gen).forSize(i) }
    }

    def map[AA >: A, B](f: AA => B): SGen[B] = SGen { i: Int => forSize(i).map(f) }
  }

  /** Exercise 12
    *
    * Implement a listOf combinator that does not accept an explicit size
    */
  object SGen {
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { i: Int => Gen.listOfN(i, g) }
  }

  object Prop {
    type MaxSize = Int
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int
    type Result = Option[(FailedCase, SuccessCount)]
  }

  case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Prop.Result)

}