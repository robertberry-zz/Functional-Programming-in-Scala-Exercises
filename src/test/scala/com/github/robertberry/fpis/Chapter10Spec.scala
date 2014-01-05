package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import Chapter10._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object Chapter10Spec {
  val arbitraryIntEndofunction = Arbitrary {
    /* some random examples */
    def increment(n: Int) = n + 1
    def decrement(n: Int) = n - 1
    def square(n: Int) = n * n
    def invert(n: Int) = -n

    Gen.oneOf(increment _, decrement _, square _, invert _)
  }

  object Laws {
    def associativity[A](monoid: Monoid[A])(implicit arbitrary: Arbitrary[A]) = forAll { (x: A, y: A, z: A) =>
      monoid.op(monoid.op(x, y), z) == monoid.op(x, monoid.op(y, z))
    }

    def identity[A](monoid: Monoid[A])(implicit arbitrary: Arbitrary[A]) = forAll { a: A =>
      monoid.op(monoid.zero, a) == a && monoid.op(a, monoid.zero) == a
    }
  }
}

class Chapter10Spec extends Specification with ScalaCheck {
  import Chapter10Spec._

  def is = "String Monoid obeys laws" ! { Laws.identity(stringMonoid) && Laws.associativity(stringMonoid) } ^
    "Addition Monoid obeys laws" ! { Laws.identity(intAddition) && Laws.associativity(intAddition) } ^
    "Multiplication Monoid obeys laws" ! { Laws.identity(intMultiplication) && Laws.associativity(intMultiplication) } ^
    "Or Monoid obeys laws" ! { Laws.identity(booleanOr) && Laws.associativity(booleanOr) } ^
    "And Monoid obeys laws" ! { Laws.identity(booleanAnd) && Laws.associativity(booleanAnd) } ^
    "First Option Monoid obeys laws" ! { Laws.identity(firstOption[Int]) && Laws.associativity(firstOption[Int]) } ^
    "Trim Monoid obeys laws" ! { Laws.identity(trimMonoid) && Laws.associativity(trimMonoid) } ^
    "Concatenate sums with addition monoid" ! prop { (xs: List[Int]) =>
      xs.isEmpty || concatenate(xs)(intAddition) == xs.sum
    } ^
    "Concatenate calculates the product with multiplication monoid" ! prop { xs: List[Int] =>
      xs.isEmpty || concatenate(xs)(intMultiplication) == xs.product
    } ^
    "foldMap _.length calculates the total length of a list of strings" ! prop { xs: List[String] =>
      foldMap(xs, intAddition)(_.length) == xs.map(_.length).sum
    } ^
    "foldMap foldLeft works as expected" ! prop { xs: List[Int] =>
      foldLeft(xs)(0)(_ - _) == xs.foldLeft(0)(_ - _)
    } ^ "foldMap foldRight works as expected" ! prop { xs: List[Int] =>
      foldRight(xs)(0)(_ - _) == xs.foldRight(0)(_ - _)
    }


  /*
    Can't run this test yet, as Scala can't determine whether two Functions are equal.

    A way to test this would be to run lots of randomly generated Ints through the resultant functions, ensuring that
    the results are the same.

    To do that we'd need to modify the Laws above to take a typeclass for testing Equality too, and then provide our own
    at this point ...

    ^
    "EndoMonoid obeys laws" ! { Laws.identity(endoMonoid[Int]) && Laws.associativity(endoMonoid[Int]) }
    */


}
