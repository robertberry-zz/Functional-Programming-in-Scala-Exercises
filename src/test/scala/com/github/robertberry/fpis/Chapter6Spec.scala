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
  }
}
