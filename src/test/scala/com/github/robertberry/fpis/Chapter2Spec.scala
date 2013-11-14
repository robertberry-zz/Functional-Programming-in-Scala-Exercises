package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import Chapter2._
import org.scalacheck.{Gen, Prop}

class Chapter2Spec extends Specification with ScalaCheck {
  def fib2(n: Int): Int = n match {
    case 1 => 1
    case 2 => 1
    case _ => fib2(n - 1) + fib2(n - 2)
  }

  def is = "fib" ! check {
    Prop.forAll(Gen.chooseNum(1, 40)) { n: Int => fib(n) == fib2(n) }
  } ^ "isSorted" ! check {
    Prop.forAll { xs: List[Int] => isSorted(xs.toArray, (x: Int, y: Int) => x > y) == (xs.sorted == xs) }
  }
}

