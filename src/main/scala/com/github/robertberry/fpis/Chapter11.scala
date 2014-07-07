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

  /** Exercise 7
    *
    * Implement compose for a Kleisli arrow
    */
  implicit class MonadExtensions4[F[_]](monad: Monad[F]) {
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = { a: A =>
      monad.flatMap(f(a)) { b =>
        g(b)
      }
    }
  }

  /** Exercise 8
    *
    * Implement flatMap in terms of compose
    */
  implicit class MonadExtensions5[F[_]](monad: Monad[F]) {
    def flatMap2[A, B](ma: F[A])(f: A => F[B]): F[B] =
      monad.compose({ _: Unit => ma }, f)(())
  }

  /** Exercise 9
    *
    * Show that the two formulations of the associative law -- one in terms of compose, one in terms of flatMap --
    * are equivalent.
    *
    * flatMap:
    *
    * x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
    *
    * compose:
    *
    * compose(compose(f, g), h) === compose(f, compose(g, h))
    *
    * --
    *
    * Start with compose law. Substitute in definition through flatMap:
    *
    * (b => (a => f(a).flatMap(g))(b).flatMap(h)) === (a => f(a).flatMap(b => g(b).flatMap(h)))
    *
    * Simplify function application:
    *
    * (b => f(b).flatMap(g).flatMap(h)) === (a => f(a).flatMap(b => g(b).flatMap(h)))
    *
    * Let f(y) === x
    *
    * (b => f(b).flatMap(g).flatMap(h))(y) === (a => f(a).flatMap(b => g(b).flatMap(h)))(y)
    *
    * Simplify function application:
    *
    * f(y).flatMap(g).flatMap(h) === f(y).flatMap(b => g(b).flatMap(h))
    *
    * x.flatMap(g).flatMap(h) === x.flatMap(b => g(b).flatMap(h))
    */

  /** Exercise 10
    *
    * Show that the two formulations of the identity law are equivalent
    *
    * flatMap:
    *
    * flatMap(x)(unit) == x
    * flatMap(unit(y))(f) == f(y)
    *
    * compose:
    *
    * compose(f, unit) == f
    * compose(unit, f) == f
    *
    * --
    *
    * Start with compose law. Substitute in definition.
    *
    * compose(f, unit) == f
    *
    * a => flatMap(f(a))(unit) == f
    *
    * Let x = f(y). Apply both sides to y.
    *
    * (a => flatMap(f(a))(unit))(y) == f(y)
    *
    * Simplify function application:
    *
    * flatMap(f(y))(unit) == f(y)
    *
    * flatMap(x)(unit) == x
    *
    * --
    *
    * Now the other side:
    *
    * compose(unit, f) == f
    *
    * Substitute in definition:
    *
    * a => flatMap(unit)(f(a)) == f
    *
    * Apply both sides to y:
    *
    * (a => flatMap(unit(a))(f))(y) == f(y)
    *
    * Simplify function application:
    *
    * flatMap(unit(y))(f) == f(y)
    */

  /** Exercise 11
    *
    * Prove that the identity laws hold for a Monad of your choice.
    *
    * --
    *
    * Using Option as it's easy peasy. :-)
    *
    * Start with the Some case.
    *
    * flatMap(Some(a))(unit) == Some(a)
    *
    * Substitute in definition of flatMap:
    *
    * unit(a) == Some(a)
    *
    * Substitute in definition of unit:
    *
    * Some(a) == Some(a)
    *
    * Now the other side:
    *
    * flatMap(unit(a))(f) == f(a)
    *
    * Substitute in definition of unit:
    *
    * flatMap(Some(a))(f) == f(a)
    *
    * Substitute in definition of flatMap:
    *
    * f(a) == f(a)
    *
    * Now let's do the None case:
    *
    * flatMap(None)(unit) == None
    *
    * Substitute in definition of flatMap:
    *
    * None == None
    */
}