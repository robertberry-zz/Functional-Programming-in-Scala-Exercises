package com.github.robertberry

package object fpis {
  def const[A, B](b: B) = (_: A) => b
}
