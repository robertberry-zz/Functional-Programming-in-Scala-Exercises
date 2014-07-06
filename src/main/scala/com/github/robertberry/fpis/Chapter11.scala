package com.github.robertberry.fpis

object Chapter11 {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }
  }

  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
    def map[A,B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))
    def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

  /** Exercise 1
    *
    * Write monad instances for Par, Parser, Option, Stream, and List.
    */
  import Chapter7.Par.Par

  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] =
      Chapter7.Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      Chapter7.Par.flatMap(ma)(f)
  }

  import Chapter9.{Parser, Parsers, Success => ParserSuccess}

  val parserMonad = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] =
      _ => ParserSuccess(a, 0)

    override def flatMap[A, B](ma: Parser[A])(f: (A) => Parser[B]): Parser[B] =
      Parsers.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] =
      Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] =
      Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] =
      List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma.flatMap(f)
  }

  /** Exercise 2
    *
    * Implement a Monad for State
    */
  import Chapter6.State

  /** FSKing type lambdas! */
  def stateMonad[S] = new Monad[({type λ[α] = State[S, α]})#λ] {
    override def unit[A](a: => A): State[S, A] = State { s =>
      (a, s)
    }

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
  }

  /** Exercise 3
    *
    * Implement sequence and traverse for monad
    */
  implicit class MonadExtensions1[F[_]](monad: Monad[F]) {
    def sequence[A](ma: List[F[A]]): F[List[A]] =
      ma.foldRight(monad.unit(List.empty[A])) { (ma, acc) =>
        monad.flatMap(ma) { a =>
          monad.map(acc) { as =>
            a :: as
          }
        }
      }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(monad.unit(List.empty[B])) { (a, acc) =>
        monad.flatMap(f(a)) { b =>
          monad.map(acc) { bs =>
            b :: bs
          }
        }
      }
  }

  /** Exercise 4
    *
    * Implement replicateM
    */
  implicit class MonadExtensions2[F[_]](monad: Monad[F]) {
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = monad.sequence(List.fill(n)(ma))
  }

  /** Exercise 5
    *
    * Describe in your own words the meaning of replicateM
    *
    * For List it gives permutations of n length of items in ma
    * For Option it gives n of what is inside the Option if defined in Some, otherwise None
    *
    * It's kind of hard to describe exactly what it does in a way that is both generic enough and doesn't also sound
    * like nonsense. I guess it replicates whatever is inside the monad n times in terms of how that would be sequenced
    * in the monad ... great explanation huh ...
    */

  /** Exercise 6
    *
    * Implement filterM
    */
  implicit class MonadExtensions3[F[_]](monad: Monad[F]) {
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms.foldRight(monad.unit(List.empty[A])) { (a, mas) =>
      monad.flatMap(f(a)) { p =>
        monad.map(mas) { as =>
          if (p) {
            a :: as
          } else {
            as
          }
        }
      }
    }
  }
}
