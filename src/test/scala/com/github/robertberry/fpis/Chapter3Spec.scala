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
  } ^ "foldLeft2" ! check {
    Prop.forAll { (as: List[Int]) =>
      foldLeft2(as, 0)(_ - _) == as.foldLeft(0)(_ - _)
    }
  } ^ "foldRight2" ! check {
    Prop.forAll { (as: List[Int]) =>
      foldRight2(as, 0)(_ - _) == as.foldRight(0)(_ - _)
    }
  } ^ "append" ! check {
    Prop.forAll { (xs: List[Int], ys: List[Int]) =>
      append(xs, ys) == xs ++ ys
    }
  } ^ "flatten" ! check {
    Prop.forAll { (xs: List[List[Int]]) =>
      flatten(xs) == xs.flatten
    }
  } ^ "add1" ! check {
    Prop.forAll { (xs: List[Int]) =>
      add1(xs) == xs.map(_ + 1)
    }
  } ^ "stringify" ! check {
    Prop.forAll { (xs: List[Double]) =>
      stringify(xs) == xs.map(_.toString)
    }
  } ^ "mapF" ! check {
    Prop.forAll { (xs: List[Int]) =>
      mapF(xs)(x => x * x) == xs.map(x => x * x)
    }
  } ^ "filterF" ! check {
    Prop.forAll { (xs: List[Int]) =>
      filterF(xs)(_ > 0) == xs.filter(_ > 0)
    }
  } ^ "flatMap" ! check {
    Prop.forAll { (xs: List[Int]) =>
      def f(n: Int) = List(n - 1, n, n + 1)

      flatMap(xs)(f) == xs.flatMap(f)
    }
  } ^ "filterF2" ! check {
    Prop.forAll { (xs: List[Int]) =>
      filterF(xs)(_ > 0) == filterF2(xs)(_ > 0)
    }
  } ^ "sum2" ! check {
    Prop.forAll { (xs: List[Int], ys: List[Int]) =>
      sum2(xs, ys) == (xs zip ys map { case ((n1, n2)) => n1 + n2 })
    }
  } ^ "map2" ! check {
    Prop.forAll { (xs: List[Int], ys: List[Int]) =>
      sum2(xs, ys) == map2(xs, ys)(_ + _)
    }
  } ^ "hasSubsequence" ! check {
    Prop.forAll { (xs: List[Int], ys: List[Int], zs: List[Int]) =>
      hasSubsequence(xs ++ ys ++ zs, ys)
    }
  }
}
