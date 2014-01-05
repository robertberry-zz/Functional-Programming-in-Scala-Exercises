package com.github.robertberry.fpis.lib

import com.github.robertberry.fpis.Chapter10._

object Strings {
  implicit class RichString(s: String) {
    def escaped = foldMap(s.toList, stringMonoid) {
      case '"' => "\\\""
      case ch => s"$ch"
    }
  }
}
