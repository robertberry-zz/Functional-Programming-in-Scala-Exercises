package com.github.robertberry.fpis

import com.github.robertberry.fpis.Chapter12.Applicative

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

  trait Monad[F[_]] extends Functor[F] with Applicative[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
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

  /** Exercise 12
    *
    * Implement join
    */
  implicit class MonadExtensions6[F[_]](monad: Monad[F]) {
    def join[A](mma: F[F[A]]): F[A] = monad.flatMap(mma)(identity)
  }

  /** Exercise 13
    *
    * Implement flatMap and compose in terms of join
    */
  implicit class MonadExtensions7[F[_]](monad: Monad[F]) {
    def flatMap3[A,B](ma: F[A])(f: A => F[B]): F[B] =
      monad.join(monad.map(ma)(f))

    def compose2[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = { a: A =>
      monad.join(monad.map(f(a))(g))
    }
  }

  /** Exercise 14
    *
    * Restate the Monad laws making use only of map, join and unit
    *
    * --
    *
    * Identity law:
    *
    * join(map(x)(unit)) == x
    * join(map(unit(y))(f)) == f(y)
    *
    * Associativity law:
    *
    * join(map(map(x)(f))(g)) == join(map(x)(a => map(f(a))(g)))
    *
    * TODO not sure about these - write proofs.
    */

  /** Exercise 15
    *
    * Explain in your own words what the associative law means for Par and Parser.
    *
    * --
    * TODO
    */

  /** Exercise 16
    *
    * Explain in your own words what the identity law means for Gen and List.
    *
    * Gen:
    *
    * That the function mapped over the Gen does not affect how the pseudo-random numbers are generated and fed into
    * subsequent Gens. ??
    *
    * List:
    *
    * That the function mapped over the List does not affect the structure of the List itself. It will neither add nor
    * remove any elements.
    */

  /** Exercise 17
    *
    * Implement Monad[Id]
    */
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] =
      ma.flatMap(f)
  }

  /** Exercise 18
    *
    * What is the meaning of replicateM for the State monad?
    *
    * How does map2 behave?
    *
    * What about sequence?
    *
    * --
    *
    * replicateM applies the stateful computation n times to the state, collecting the results into a list.
    *
    * map2 threads the state through the first stateful computation and then the second, returning the result of
    * applying f to the two results of those computations.
    *
    * sequence threads the state through the list of stateful computations, collecting the results into a list.
    */

  /** Exercise 19
    *
    * What laws do you expect to mutually hold for getState, setState, unit and flatMap
    *
    * getState.flatMap(setState) == unit(())
    *
    * TODO more
    */

  /** Exercise 20
    *
    * Implement the monad instance for Reader.
    */
  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: => A): Reader[R, A] =
        Reader(_ => a)

      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader { r: R =>
        f(st.run(r)).run(r)
      }
    }
  }

  /** Exercise 20 (cont.)
    *
    * What does it mean?
    * --
    * It's a function that reads a value and produces something else. As none of the primitive operations are able to
    * change or create an R, it must feed the same value through throughout.
    *
    *
    * What are its primitive operations?
    * --
    * get = Reader(identity) would just return the value.
    *
    * What is the meaning of flatMap?
    * --
    *
    * Feeds the argument into the first reader to get an a, which is then fed to f to produce another reader, into which
    * is fed the argument again, to produce the final value.
    *
    * What meaning does it give to sequence, join, and replicateM?
    * --
    * Sequence creates a reader that takes a list of readers, feeds the argument into each one, and returns a list of
    * the results.
    *
    * Join takes a reader that produces a reader, and creates a new reader that feeds the argument to that, then to
    * the resultant reader, to get the final value.
    *
    * replicateM creates a reader the same as the given reader except it produces the resultant value n times in a list.
    *
    * i.e., listMonad.replicateM(n, r.run(x)) == readerMonad.replicateM(n, r).run(x)
    *
    *
    * What meaning does it give to the monad laws?
    * --
    *
    * TODO
    */
}