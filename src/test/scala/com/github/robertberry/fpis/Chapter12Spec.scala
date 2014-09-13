package com.github.robertberry.fpis

import com.github.robertberry.fpis.Chapter12._
import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary
import org.specs2.{ScalaCheck, Specification}

class Chapter12Spec extends Specification with ScalaCheck {
  implicit val arbitraryShortListInt: Arbitrary[List[Int]] = Arbitrary {
    for {
      sizeOfList <- Gen.chooseNum(0, 5)
      ints <- Gen.listOfN(sizeOfList, arbitrary[Int])
    } yield ints
  }

  def reverseSpec[F[_], A](implicit M: Arbitrary[F[A]], N: Traverse[F]) = check { (a: F[A], b: F[A]) =>
    N.toList(N.reverse(a)) ++ N.toList(N.reverse(b)) == (N.toList(a) ++ N.toList(b)).reverse
  }

  def is = "reverseList" ! reverseSpec[List, Int]
}
