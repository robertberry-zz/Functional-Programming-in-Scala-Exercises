package com.github.robertberry.fpis

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck.{Prop, Gen, Arbitrary}
import Arbitrary._
import Chapter5._

trait ArbitraryStream {
  def arbitraryCons[A: Arbitrary]: Arbitrary[Stream[A]] = Arbitrary {
    for {
      h <- arbitrary[A]
      t <- arbitrary[Stream[A]]
    } yield Stream.cons(h, t)
  }

  implicit def arbitraryStream[A: Arbitrary]: Arbitrary[Stream[A]] = Arbitrary {
    for {
      isEmpty <- arbitrary[Boolean]
      stream <- if (isEmpty) Gen.const(Stream.empty[A]) else arbitraryCons[A].arbitrary
    } yield stream
  }
}

class Chapter5Spec extends Specification with ScalaCheck with ArbitraryStream {
  def is = "toList" ! check {
    Prop.forAll { (xs: List[Int]) =>
      toStream(xs).toList == xs
    }
  } ^ "take" ! check {
    Prop.forAll { (xs: Stream[Int], n: Int) =>
      xs.take(n).toList == xs.toList.take(n)
    }
  } ^ "takeWhile" ! check {
    Prop.forAll { (xs: Stream[Int], n: Int) =>
      def f(i: Int) = i > n

      xs.takeWhile(f).toList == xs.toList.takeWhile(f)
    }
  } ^ "forAll" ! check {
    Prop.forAll { (xs: Stream[Int], n: Int) =>
      def f(i: Int) = i > n

      xs.forAll(f) == xs.toList.forall(f)
    }
  } ^ "takeWhile2" ! check {
    Prop.forAll { (xs: Stream[Int], n: Int) =>
      def f(i: Int) = i > n

      xs.takeWhile2(f).toList == xs.toList.takeWhile(f)
    }
  } ^ "uncons2" ! check {
    Prop.forAll { (xs: Stream[Int]) =>
    /** Because we're not using case classes for this (and can't really, due to the nature of the lazy computations),
      * no equality method is defined on the Stream itself
      */
      xs.uncons.isDefined == xs.uncons2.isDefined
    }
  } ^ "foldRight2" ! check {
    Prop.forAll { (xs: Stream[Int]) =>
      xs.foldRight(0)(_ - _) == xs.foldRight2(0)(_ - _)
    }
  } ^ "map" ! check {
    Prop.forAll { (xs: Stream[Int]) =>
      def f(i: Int) = i * i

      xs.map(f).toList == xs.toList.map(f)
    }
  } ^ "filter" ! check {
    def f(i: Int) = i > 0

    Prop.forAll { (xs: Stream[Int]) =>
      xs.filter(f).toList == xs.toList.filter(f)
    }
  } ^ "append" ! check {
    Prop.forAll { (xs: Stream[Int], ys: Stream[Int]) =>
      xs.append(ys).toList == xs.toList ++ ys.toList
    }
  } ^ "flatMap" ! check {
    def f(i: Int) = Stream.cons(i, Stream.cons(i + 1, Stream.empty))

    Prop.forAll { (xs: Stream[Int]) =>
      xs.flatMap(f).toList == xs.toList.flatMap(f(_).toList)
    }
  } ^ "constant" ! check {
    Prop.forAll { (x: Int) =>
      Prop.forAll(Gen.chooseNum(0, 50)) { (n: Int) =>
        constant(x).take(n).toList == List.fill(n)(x)
      }
    }
  } ^ "from" ! check {
    Prop.forAll { (x: Int) =>
      Prop.forAll(Gen.chooseNum(1, 50)) { (n: Int) =>
        def listFrom(x: Int, n: Int, acc: List[Int] = Nil): List[Int] =
          if (n == 0) acc.reverse else listFrom(x + 1, n - 1, x :: acc)
        from(x).take(n).toList == listFrom(x, n)
      }
    }
  } ^ "drop" ! check {
    Prop.forAll(Gen.chooseNum(0, 50)) { (n: Int) =>
      Prop.forAll { (xs: Stream[Int]) =>
        xs.drop(n).toList == xs.toList.drop(n)
      }
    }
  } ^ "zipWith" ! check {
    Prop.forAll { (xs: Stream[Int], ys: Stream[Int]) =>
      zipWith(xs, ys)(_ + _).toList == xs.toList.zip(ys.toList).map({ case ((x: Int, y: Int)) => x + y })
    }
  } ^ "fibs" ! {
    val firstTenFibs = fibs.take(10).toList

    (for {
      ((i, j), k) <- firstTenFibs.zip(firstTenFibs.drop(1)).zip(firstTenFibs.drop(2))
    } yield (i + j) mustEqual k).reduce(_ and _)
  } ^ "fibs2" ! {
    fibs2.take(20).toList == fibs.take(20).toList
  } ^ "constant2" ! check {
    Prop.forAll { i: Int =>
      constant2(i).take(30).forAll(_ == i)
    }
  } ^ "ones2" ! {
    ones2.take(50).forAll(_ == 1)
  } ^ "startsWith" ! check {
    Prop.forAll { (xs: Stream[Int], ys: Stream[Int]) =>
      startsWith(xs.append(ys), xs) && startsWith(xs, xs) && (xs.isEmpty || !startsWith(xs, xs.map(_ + 1)))
    }
  } ^ "tails" ! check {
    Prop.forAll { (xs: Stream[Int]) =>
      tails(xs).toList.length == xs.toList.length
    }
  } ^ "hasSubsequence" ! check {
    Prop.forAll { (xs: Stream[Int], ys: Stream[Int], zs: Stream[Int]) =>
      val appended = xs.append(ys).append(zs)
      appended.isEmpty || hasSubsequence(appended, xs) && hasSubsequence(appended, ys) && hasSubsequence(appended, zs)
    }
  } ^ "scanRight" ! check {
    Prop.forAll { (xs: Stream[Int]) =>
      scanRight(xs)(0)(_ + _).toList == xs.toList.tails.map(_.sum).toList
    }
  }
}
