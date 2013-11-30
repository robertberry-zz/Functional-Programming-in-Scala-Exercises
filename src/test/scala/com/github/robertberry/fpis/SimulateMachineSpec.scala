package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck._
import org.scalacheck.Arbitrary._
import Chapter6._

trait ArbitraryInput {
  implicit val arbitraryInput: Arbitrary[Input] = Arbitrary(Gen.oneOf(Coin, Turn))
}

trait ArbitraryMachine {
  implicit val arbitraryMachine = Arbitrary {
    for {
      locked <- arbitrary[Boolean]
      candies <- Gen.chooseNum(0, 50)
      coins <- Gen.chooseNum(0, 50)
    } yield Machine(locked, candies, coins)
  }
}

trait Helpers {
  implicit class RichList[A](as: List[A]) {
    def squashDuplicates = as match {
      case h :: t => h :: t.foldLeft((h, List.empty[A])) {
        case ((lastSeen, acc), a) => (a, if (a == lastSeen) acc else a :: acc)
      }._2.reverse
      case Nil => Nil
    }
  }

  def complement[A](f: A => Boolean): A => Boolean = a => !f(a)

  val isCoin: (Input => Boolean) = { case Coin => true; case Turn => false }
  val isTurn = complement(isCoin)
}

class SimulateMachineSpec extends Specification with ScalaCheck with ArbitraryInput with ArbitraryMachine with Helpers {
  def is = "consistency between state and return value" ! check {
    Prop.forAll { (machine: Machine, inputs: List[Input]) =>    
      val ((coins, candies), finalState) = simulateMachine(inputs).run(machine)

      finalState.coins == coins && finalState.candies == candies
    }
  } ^ "final coin count" ! check {
    Prop.forAll { (machine: Machine, inputs: List[Input]) => 
      val ((coins, _), _) = simulateMachine(inputs).run(machine)
      val coinsInserted = inputs.count(isCoin)
      coins == machine.coins + coinsInserted
    }
  } ^ "final candy count" ! check {
    Prop.forAll { (machine: Machine, inputs: List[Input]) =>
      val ((_, candies), _) = simulateMachine(inputs).run(machine)

      val pairs = inputs.squashDuplicates.dropWhile(isTurn).grouped(2).takeWhile(_.length == 2)

      candies == Math.max(0,
        machine.candies - ((if (!machine.locked && inputs.headOption.exists(isTurn)) 1 else 0) + pairs.length))
    }
  }
}
