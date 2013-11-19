package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck._
import org.scalacheck.Arbitrary._
import Chapter4._

trait ArbitraryMaybes {
  implicit def arbitraryJust[A: Arbitrary]: Arbitrary[Just[A]] = Arbitrary {
    for {
      a <- arbitrary[A]
    } yield Just(a)
  }

  implicit def arbitraryMaybe[A: Arbitrary]: Arbitrary[Maybe[A]] = Arbitrary {
    for {
      isDefined <- arbitrary[Boolean]
      maybe <- if (isDefined) arbitrary[Just[A]] else Gen.const(Absent)
    } yield maybe
  }
}

class Chapter4Spec extends Specification with ScalaCheck with ArbitraryMaybes {
  def is = "map" ! check {
    Prop.forAll { (maybe: Maybe[Int]) => maybe match {
        case Absent => maybe.map(x => x * x) == Absent
        case Just(x) => maybe.map(x => x * x) == Just(x * x)
      }
    }
  } ^ "flatMap" ! check {
    Prop.forAll { (m1: Maybe[Int], definedF: Boolean) =>
      def f(i: Int) = if (!definedF) Absent else Just(i * i)

      m1 match {
        case Absent => m1.flatMap(f) == Absent
        case _ if !definedF => m1.flatMap(f) == Absent
        case Just(x) => m1.flatMap(f) == Just(x * x)
      }
    }
  } ^ "getOrElse" ! check {
    Prop.forAll { (maybe: Maybe[Int], default: Int) =>
      maybe match {
        case Absent => maybe.getOrElse(default) == default
        case Just(x) => maybe.getOrElse(default) == x
      }
    }
  } ^ "orElse" ! check {
    Prop.forAll { (m1: Maybe[Int], m2: Maybe[Int]) =>
      m1 match {
        case Absent => (m1 orElse m2) == m2
        case _ => (m1 orElse m2) == m1
      }
    }
  } ^ "filter" ! check {
    Prop.forAll { (maybe: Maybe[Int]) =>
      def isOdd(n: Int) = n % 2 == 1

      maybe match {
        case Absent => maybe.filter(isOdd) == Absent
        case Just(x) => if (isOdd(x)) maybe.filter(isOdd) == maybe else maybe.filter(isOdd) == Absent
      }
    }
  } ^ "variance" ! check {
    Prop.forAll { (xs: Seq[Double]) =>
      if (xs.isEmpty) {
        variance(xs) == Absent
      } else {
        val meanOfXs = xs.sum / xs.length
        val vari = xs.map(x => math.pow(x - meanOfXs, 2)).sum / xs.length

        variance(xs) == Just(vari)
      }
    }
  } ^ "map2" ! check {
    Prop.forAll { (a: Maybe[Int], b: Maybe[Int]) =>
      def f(x: Int, y: Int) = x - y

      map2(a, b)(f) == (if (a.isDefined && b.isDefined) Just(f(a.get, b.get)) else Absent)
    }
  } ^ "sequence" ! check {
    Prop.forAll { (as: List[Maybe[Int]]) =>
      if (as.exists(!_.isDefined)) {
        sequence(as) == Absent
      } else {
        sequence(as) == Just(as.map(_.get))
      }
    }
  } ^ "sequence2" ! check {
    Prop.forAll { (as: List[Maybe[Int]]) =>
      sequence2(as) == sequence(as)
    }
  }
}
