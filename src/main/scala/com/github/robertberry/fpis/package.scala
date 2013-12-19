package com.github.robertberry

import scala.util.Try

package object fpis {
  def const[A, B](b: B) = (_: A) => b

  object IntegerString {
    def unapply(s: String) = Try {
      s.toInt
    }.toOption
  }
}
