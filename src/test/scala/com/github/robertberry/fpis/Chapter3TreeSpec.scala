package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import Chapter3._
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbitraryTrees {
  implicit def arbitraryLeaf[A: Arbitrary]: Arbitrary[Leaf[A]] = Arbitrary {
    for {
      a <- arbitrary[A]
    } yield Leaf(a)
  }

  implicit def arbitraryBranch[A: Arbitrary]: Arbitrary[Branch[A]] = Arbitrary {
    for {
      left <- arbitrary[Tree[A]]
      right <- arbitrary[Tree[A]]
    } yield Branch(left, right)
  }

  implicit def arbitraryTree[A: Arbitrary]: Arbitrary[Tree[A]] = Arbitrary {
    for {
      isLeaf <- arbitrary[Boolean]
      tree <- if (isLeaf) arbitrary[Leaf[A]] else arbitrary[Branch[A]]
    } yield tree
  }
}

trait TreeHelpers {
  def flattenTree[A](tree: Tree[A]): List[A] = tree match {
    case Branch(left, right) => flattenTree(left) ++ flattenTree(right)
    case Leaf(a) => List(a)
  }
}

class Chapter3TreeSpec extends Specification with ScalaCheck with ArbitraryTrees with TreeHelpers {
  def is = "sizeT" ! {
    /** Couldn't think of a reasonable way to test this with ScalaCheck without basically just rewriting my code as
      * Scala is already so declarative
      */
    List(
      Leaf(1) -> 1,
      Branch(Leaf(1), Leaf(1)) -> 3,
      Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)) -> 5,
      Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) -> 7
    ) forall { case (tree, expectedSize) => sizeT(tree) == expectedSize }
  } ^ "maximumT" ! check {
    Prop.forAll { (tree: Tree[Int]) =>
      maximumT(tree) == flattenTree(tree).max
    }
  } ^ "depthT" ! check {
    List(
      Leaf(1) -> 1,
      Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)) -> 3,
      Branch(Leaf(1), Branch(Leaf(1), Leaf(1))) -> 3,
      Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Leaf(1)) -> 4
    ) forall { case (tree, expectedDepth) => depthT(tree) == expectedDepth }
  } ^ "mapT" ! check {
    Prop.forAll { (tree: Tree[Int]) =>
      flattenTree(mapT(tree)(x => x * x)) == flattenTree(tree).map(x => x * x)
    }
  } ^ "fSize" ! check {
    Prop.forAll { (tree: Tree[Int]) =>
      fSize(tree) == sizeT(tree)
    }
  } ^ "fMax" ! check {
    Prop.forAll { (tree: Tree[Int]) =>
      fMax(tree) == maximumT(tree)
    }
  } ^ "fDepth" ! check {
    Prop.forAll { (tree: Tree[Int]) =>
      fDepth(tree) == depthT(tree)
    }
  } ^ "fMap" ! check {
    Prop.forAll { (tree: Tree[Int]) =>
      fMap(tree)(x => x * x) == mapT(tree)(x => x * x)
    }
  }

}
