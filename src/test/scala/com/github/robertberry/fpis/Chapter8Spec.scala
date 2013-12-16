package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.{Gen, Arbitrary}
import org.specs2.specification.Fragments
import Chapter8._

trait Ex3Prop {
  private val trueProp = new Ex3.Prop {
    def check: Boolean = true
  }

  private val falseProp = new Ex3.Prop {
    def check: Boolean = false
  }

  implicit val arbitraryEx3Prop = Arbitrary { Gen.oneOf(trueProp, falseProp) }
}

class Chapter8Spec extends Specification with ScalaCheck with Ex3Prop {
  def is: Fragments = "&&" ! prop { (prop1: Ex3.Prop, prop2: Ex3.Prop) =>
    (prop1.check && prop2.check) == (prop1 && prop2).check
  }
}
