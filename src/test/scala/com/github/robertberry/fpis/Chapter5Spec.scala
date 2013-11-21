package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck.{Prop, Gen, Arbitrary}
import Arbitrary._
import Chapter5._

trait ArbitraryStream {
  def arbitraryCons[A: Arbitrary]: Arbitrary[Stream[A]] = Arbitrary {
    for {
      h <- arbitrary[A]
      t <- arbitrary[Stream[A]]
    } yield Stream.cons(h, t)
  }

  implicit def arbitraryStream[A: Arbitrary]: Arbitrary[Stream[A]] = Arbitrary {
    for {
      isEmpty <- arbitrary[Boolean]
      stream <- if (isEmpty) Gen.const(Stream.empty[A]) else arbitraryCons[A].arbitrary
    } yield stream
  }
}

class Chapter5Spec extends Specification with ScalaCheck with ArbitraryStream {
  def is = "toList" ! check {
    Prop.forAll { (xs: List[Int]) =>
      toStream(xs).toList == xs
    }
  } ^ "take" ! check {
    Prop.forAll { (xs: List[Int], n: Int) =>
      toStream(xs).take(n).toList == xs.take(n)
    }
  } ^ "takeWhile" ! check {
    Prop.forAll { (xs: List[Int], n: Int) =>
      def f(i: Int) = i > n

      toStream(xs).takeWhile(f).toList == xs.takeWhile(f)
    }
  }
}
