package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import org.specs2.specification.Fragments
import org.scalacheck.{Gen, Arbitrary, Prop}
import org.scalacheck.Arbitrary._
import Chapter4._

trait ArbitraryEither {
  implicit def arbitraryLeft[A: Arbitrary] = Arbitrary {
    for {
      a <- arbitrary[A]
    } yield Left2(a)
  }

  implicit def arbitraryRight[A: Arbitrary] = Arbitrary {
    for {
      a <- arbitrary[A]
    } yield Right2(a)
  }

  implicit def arbitraryEither[A: Arbitrary, B: Arbitrary]: Arbitrary[Either2[A, B]] = Arbitrary {
    Gen.oneOf(arbitrary[Left2[A]], arbitrary[Right2[B]])
  }
}

class Chapter4EitherSpec extends Specification with ScalaCheck with ArbitraryEither {
  def is: Fragments = "leftMap" ! check {
    Prop.forAll { (either: Left2[Int]) =>
      either.map((_: Int) + 1) == either
    }
  } ^ "rightMap" ! check {
    Prop.forAll { (i: Int) =>
      val either = Right2(i)
      either.map(_ + 1) == Right2(i + 1)
    }
  } ^ "leftFlatMap" ! check {
    Prop.forAll { (either: Left2[Int]) =>
      either.map((_: Int) + 1) == either
    }
  } ^ "rightFlatMap1" ! check {
    Prop.forAll { (i: Int, error: String) =>
      Right2(i).flatMap(_ => Left2(error)) == Left2(error)
    }
  } ^ "rightFlatMap2" ! check {
    Prop.forAll { (i: Int, j: Int) =>
      Right2(i).flatMap(x => Right2(x + j)) == Right2(i + j)
    }
  } ^ "orElse1" ! check {
    Prop.forAll { (left: Left2[String], either: Either2[String, Int]) =>
      left.orElse(either) == either
    }
  } ^ "orElse2" ! check {
    Prop.forAll { (right: Right2[Int], either: Either2[String, Int]) =>
      right.orElse(either) == right
    }
  } ^ "map21" ! check {
    Prop.forAll { (left: Left2[String], either: Either2[String, Int]) =>
      left.map2(either)((_: Int) + (_: Int)) == left
    }
  } ^ "map22" ! check {
    Prop.forAll { (right: Right2[Int], left: Left2[Int]) =>
      right.map2(left)((_: Int) + (_: Int)) == left
    }
  } ^ "map23" ! check {
    Prop.forAll { (i: Int, j: Int) =>
      Right2(i).map2(Right2(j))((_: Int) + (_: Int)) == Right2(i + j)
    }
  } ^ "sequence" ! check {
    Prop.forAll { (eithers: List[Either2[String, Int]]) =>
      eithers.collectFirst({ case left @ Left2(_) => left }) match {
        case Some(error) => sequence3(eithers) == error
        case _ => sequence3(eithers) == Right2(eithers.collect({ case Right2(i) => i }))
      }
    }
  } ^ "traverse" ! check {
    Prop.forAll { (xs: List[Int]) =>
      /** Silly arbitrary function here ... */
      val err = Left2("Only defined for positive integers")
      def f(i: Int) = if (i < 0) err else Right2(i + 1)

      val traversed = traverse3(xs)(f)

      if (xs.exists(_ < 0)) traversed == err else traversed == Right2(xs.map(_ + 1))
    }
  }
}
