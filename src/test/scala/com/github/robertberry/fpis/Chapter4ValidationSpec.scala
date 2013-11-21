package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck.{Prop, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import Chapter4._

trait ArbitraryValidations {
  implicit def arbitraryErrorTrail[A: Arbitrary] = Arbitrary {
    for {
      errors <- arbitrary[List[A]]
    } yield ErrorTrail(errors)
  }

  implicit def arbitrarySuccess[A: Arbitrary] = Arbitrary {
    for {
      a <- arbitrary[A]
    } yield Validated(a)
  }

  implicit def arbitraryValidation[A: Arbitrary, B: Arbitrary]: Arbitrary[Validation[A, B]] = Arbitrary {
    for {
      isSuccess <- arbitrary[Boolean]
      validation <- if (isSuccess) arbitrary[Validated[B]] else arbitrary[ErrorTrail[A]]
    } yield validation
  }
}

class Chapter4ValidationSpec extends Specification with ScalaCheck with ArbitraryValidations {
  def is = "orElse1" ! check {
    Prop.forAll { (errorTrail: ErrorTrail[String], success: Validated[Int]) =>
      (errorTrail orElse success) == success
    }
  } ^ "orElse2" ! check {
    Prop.forAll { (success: Validated[Int], validation: Validation[String, Int]) =>
      (success orElse validation) == success
    }
  } ^ "orElse3" ! check {
    Prop.forAll { (e1s: List[String], e2s: List[String]) =>
      (ErrorTrail(e1s) orElse ErrorTrail(e2s)) == ErrorTrail(e1s ++ e2s)
    }
  } ^ "sequence1" ! check {
    Prop.forAll { (xs: List[Int]) =>
      sequence4(xs.map(Validated(_))) == Validated(xs)
    }
  } ^ "sequence2" ! check {
    Prop.forAll { (validations: List[Validation[String, Int]]) =>
      val errors = validations collect { case ErrorTrail(errs) => errs }

      errors.isEmpty || (sequence4(validations) == ErrorTrail(errors.flatten))
    }
  }
}
