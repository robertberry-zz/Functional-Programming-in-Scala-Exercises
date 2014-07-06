package com.github.robertberry.fpis

import org.specs2.Specification
import org.specs2.specification.Fragments
import Chapter11._

import scala.util.Try

class Chapter11Spec extends Specification {
  override def is: Fragments = "optionMonad unit" ! {
    optionMonad.unit(1) mustEqual Some(1)
  } ^ "optionMonad flatMap" ! {
    optionMonad.flatMap(Some(1)) { a =>
      Some(a + 5)
    } mustEqual Some(6)
  }
}

class MonadExtensions1Spec extends Specification {
  override def is: Fragments = "sequence over Option (no None)" ! {
    optionMonad.sequence(List(Some(1), Some(2), Some(3))) mustEqual Some(List(1, 2, 3))
  } ^ "sequence over Option (with None)" ! {
    optionMonad.sequence(List(Some(1), None, Some(3))) mustEqual None
  } ^ "sequence over List" ! {
    listMonad.sequence(List(List(1, 2, 3), List(4, 5, 6))) mustEqual List(
      List(1, 4),
      List(1, 5),
      List(1, 6),
      List(2, 4),
      List(2, 5),
      List(2, 6),
      List(3, 4),
      List(3, 5),
      List(3, 6)
    )
  } ^ "traverse over Option (no None)" ! {
    optionMonad.traverse(List("1", "2", "3")) { s =>
      Try {
        s.toInt
      }.toOption
    } mustEqual Some(List(1, 2, 3))
  } ^ "traverse over Option (with None)" ! {
    optionMonad.traverse(List("1", "a2", "3")) { s =>
      Try {
        s.toInt
      }.toOption
    } mustEqual None
  }
}