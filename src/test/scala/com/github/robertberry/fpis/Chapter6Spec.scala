package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import Chapter6._

trait ArbitraryRNG {
  implicit val arbitrarySimple = Arbitrary {
    for {
      n <- arbitrary[Long]
    } yield Simple(n)
  }
}

class Chapter6Spec extends Specification with ScalaCheck with ArbitraryRNG {
  def is = "positiveInt" ! check {
    Prop.forAll { (rng: Simple) =>
      positiveInt(rng)._1 >= 0
    }
  } ^ "double" ! check {
    Prop.forAll { (rng: Simple) =>
      val d = double(rng)._1
      d >= -1 && d <= 1
    }
  } ^ "ints" ! check {
    Prop.forAll(Gen.chooseNum(0, 50)) { (i: Int) =>
      Prop.forAll { (rng: Simple) =>
        val (randomInts, rng2) = ints(i)(rng)

        /** Nth state of RNG */
        def nthIterRng(n: Int, rng: RNG): RNG = if (n == 0) rng else {
          val (_, rng2) = rng.nextInt
          nthIterRng(n - 1, rng2)
        }

        randomInts.size == i && rng2 == nthIterRng(i, rng)
      }
    }
  } ^ "mapDouble" ! check {
    Prop.forAll { (rng: Simple) =>
      double(rng) == Rand.double(rng)
    }
  } ^ "map2" ! check {
    Prop.forAll { (rng: Simple) =>
      val ((i, d), newRng) = Rand.map2(Rand.int, Rand.double)((_, _))(rng)

      val (i2, rng2) = Rand.int(rng)
      val (d2, rng3) = Rand.double(rng2)

      i == i2 && d == d2 && rng3 == newRng
    }
  } ^ "sequence" ! check {
    Prop.forAll { (rng: Simple) =>
      Prop.forAll(Gen.chooseNum(0, 50)) { (i: Int) =>
        Rand.ints(i)(rng) == ints(i)(rng)
      }
    }
  }
}
