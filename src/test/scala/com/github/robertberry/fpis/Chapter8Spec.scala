package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.{Gen, Arbitrary, Prop}
import org.specs2.specification.Fragments
import Chapter8._
import com.github.robertberry.fpis.Chapter6.Simple

trait Ex3Prop {
  private val trueProp = new Ex3.Prop {
    def check: Boolean = true
  }

  private val falseProp = new Ex3.Prop {
    def check: Boolean = false
  }

  implicit val arbitraryEx3Prop = Arbitrary { Gen.oneOf(trueProp, falseProp) }
}

class Chapter8Spec extends Specification with ScalaCheck with Ex3Prop with ArbitraryRNG {
  val g1 = Chapter8.Gen.choose(-20, -10)
  val g2 = Chapter8.Gen.choose(10, 20)

  val passProp = Chapter8.Prop { (_, _) => None }
  val failProp = Chapter8.Prop { (_, _) => Some(("Failed", 0)) }

  def is: Fragments = "&&" ! prop { (prop1: Ex3.Prop, prop2: Ex3.Prop) =>
    (prop1.check && prop2.check) == (prop1 && prop2).check
  } ^ "choose" ! prop { (rng: Simple, start: Int, stopExclusive: Int) =>
    // this covers the case where stop is before start, or when the range is too large and overflows
    stopExclusive <= start || stopExclusive - Int.MaxValue > start || {
      val (n, _) = Chapter8.Gen.choose(start, stopExclusive).sample.run(rng)
      n >= start && n < stopExclusive
    }
  } ^ "unit" ! prop { (rng: Simple, n: Int) =>
    Chapter8.Gen.unit(n).sample.run(rng)._1 == n
  } ^ "listOfN" ! prop { rng: Simple =>
    Prop.forAll(Gen.chooseNum(0, 50)) { n: Int =>
      val (xs, _) = Chapter8.Gen.listOfN(n, Chapter8.Gen.boolean).sample.run(rng)
      xs.length == n
    }
  } ^ "sameParity" ! prop { rng: Simple =>
    val ((n, m), _) = Chapter8.Gen.sameParity(-1000, 1000).sample.run(rng)
    (n >= 0) == (m >= 0)
  } ^ "listOfN2" ! prop { rng: Simple =>
    val (xs, _) = Chapter8.Gen.listOfN2(Chapter8.Gen.choose(0, 100), Chapter8.Gen.boolean).sample.run(rng)
    xs.length >= 0 && xs.length < 100
  } ^ "union" ! prop { rng: Simple =>
    val (x, _) = Chapter8.Gen.union(g1, g2).sample.run(rng)
    (x >= -20 && x < -10) || (x >= 10 && x < 20)
  } ^ "weighted left bias" ! prop { rng: Simple =>
    val (leftBias, _) = Chapter8.Gen.weighted((g1, 1), (g2, 0)).sample.run(rng)
    leftBias >= -20 && leftBias < -10
  } ^ "weighted right bias" ! prop { rng: Simple =>
    val (rightBias, _) = Chapter8.Gen.weighted((g1, 0), (g2, 1)).sample.run(rng)
    rightBias >= 10 && rightBias < 20
  } ^ "weighted" ! prop { rng: Simple =>
    val (x, _) = Chapter8.Gen.weighted((g1, 1), (g2, 1)).sample.run(rng)
    (x >= -20 && x < -10) || (x >= 10 && x < 20)
  } ^ "Prop &&" ! prop { rng: Simple =>
    (passProp && passProp).run(1, rng).isEmpty &&
      (failProp && failProp).run(1, rng).isDefined &&
      (failProp && passProp).run(1, rng).isDefined &&
      (passProp && failProp).run(1, rng).isDefined
  } ^ "Prop ||" ! prop { rng: Simple =>
    (passProp || passProp).run(1, rng).isEmpty &&
      (passProp || failProp).run(1, rng).isEmpty &&
      (failProp || passProp).run(1, rng).isEmpty &&
      (failProp || failProp).run(1, rng).isDefined
  }
}
