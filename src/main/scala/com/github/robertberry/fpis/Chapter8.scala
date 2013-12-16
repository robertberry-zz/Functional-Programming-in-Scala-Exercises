package com.github.robertberry.fpis

object Chapter8 {
  /** Exercise 1
    *
    * Try thinking of properties to specify the implementation of a sum: List[Int] => Int function
    *
    * - If List[Int] is split to h :: t, sum(h :: t) == h + sum(t)
    * - More generally, if xs is split into as ++ bs, sum(as) ++ sum(bs) == sum(xs)
    * - If x is the same value for all xs, x * len(xs) == sum(xs)
    * - sum(-sum(xs) :: xs) == 0
    */

  /** Exercise 2
    *
    * What are properties that specifies a function that finds the maximum of a List[Int]
    *
    * - max(xs) == max(reverse(xs))
    * - given xs == as ++ bs, max(List(max(as), max(bs))) == max(xs)
    * - If x is the same value for all xs, max(xs) == x
    * - If x is the same value for all xs, max((x - 1) :: xs) == x
    * - If x is the same value for all xs, max((x + 1) :: xs) == x + 1
    */

  /** Exercise 3
    *
    * Assuming the following definition of Prop, implement &&
    */
  object Ex3 {
    trait Prop { p1 =>
      def check: Boolean

      def &&(p2: Prop) = new Prop {
        def check: Boolean = p1.check && p2.check
      }
    }
  }

  object Prop {
    type SuccessCount = Int
  }





}