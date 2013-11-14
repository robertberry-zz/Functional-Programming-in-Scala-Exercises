package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Arbitrary._
import Chapter3._

class Chapter3Spec extends Specification with ScalaCheck {
  def is = "tail" ! check {
    Prop.forAll { (h: Int, t: List[Int]) =>
      tail(h :: t) == t
    }
  } ^ "setHead" ! check {
    Prop.forAll { (a: Int, h: Int, t: List[Int]) =>
      setHead(a, h :: t) == a :: t
    }
  } ^ "drop" ! check {
    Prop.forAll(Gen.posNum[Int]) { (a: Int) =>
      Prop.forAll(Gen.containerOfN[List, Int](a, arbitrary[Int]), arbitrary[List[Int]]) { (front: List[Int], back: List[Int]) =>
        drop(front ++ back, a) == back
      }
    }
  } ^ "dropWhile" ! check {
    Prop.forAll { (as: List[Int]) =>
      def gtZero(i: Int) = i > 0
      dropWhile(as, gtZero) == as.dropWhile(gtZero)
    }
  } ^ "init" ! check {
    Prop.forAll { (as: List[Int], a: Int) =>
      init(as ++ List(a)) == as
    }
  } ^ "length" ! check {
    Prop.forAll { (as: List[Int]) => lengthRight(as) == as.length }
  } ^ "foldLeft" ! check {
    Prop.forAll { (as: List[Int]) =>
      foldLeft(as, 0)(_ + _) == as.foldLeft(0)(_ + _) &&
      foldLeft(as, 1)(_ * _) == as.foldLeft(1)(_ * _)
    }
  } ^ "sum" ! check {
    Prop.forAll { (h: Int, t: List[Int]) =>
      sum(h :: t) == (h :: t).sum
    }
  } ^ "product" ! check {
    def testProduct(as: List[Int]): Int = as match {
      case Nil => 1
      case n :: rest => n * testProduct(rest)
    }
    Prop.forAll { (as: List[Int]) =>
      product(as) == testProduct(as)
    }
  } ^ "length" ! check {
    Prop.forAll { (as: List[Int]) =>
      lengthLeft(as) == as.length
    }
  } ^ "reverse" ! check {
    Prop.forAll { (as: List[Int]) =>
      reverse(as) == as.reverse
    }
  }
}
