package com.chriniko.essential.scala.cats_tutorial

import cats.data.{EitherT, Kleisli, OptionT}
import cats.{Foldable, Functor, Monad, MonoidK, SemigroupK}
import cats.kernel.Semigroup

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}

object test {


  def main(args: Array[String]): Unit = {

    // ---
    println()

    {

      val c1 = ComplexNumber(1, 2)
      val c2 = ComplexNumber(2, 3)

      import ComplexNumber._
      val c3 = SuperAdder.add(c1, c2)

      println(c3)
    }

    // ---
    println()

    {

      import ListImplicits._
      val r = listSemigroupK.combineK(List(1, 2, 3), List("a", "b", "c"))
      println(r)

      import OptionImplicits._
      val r2 = optionSemigroupK.combineK(None, Some(2))
      println(r2)

    }

    // ---
    println()

    {
      import cats.implicits._

      println(Semigroup[Int].combine(1, 2))
      println(SemigroupK[List].combineK(List(Some(1), Some(2)), List(Some(1))))
      println(SemigroupK[List].combineK(List(Some(1), Some(2)), List(None)))


      val r = List(1, 2, 3) |+| List(1, 2)
      println(r)

      val r2 = List(Some(1), None, Some(3)) |+| List(Some(1), None)
      println(r2)
    }

    // --
    println()

    {
      type NEL[A] = (A, List[A])

      // some things could not be expressed as monoids
      val nelMonoid = new MonoidK[NEL] {

        override def empty[A]: (A, List[A]) = ???

        override def combineK[A](x: (A, List[A]), y: (A, List[A])): (A, List[A]) = {
          (x._1, (x._2 :+ y._1) ++ y._2)
        }
      }

    }


    // ---
    println()


    {
      import cats.implicits._

      println(Foldable[List].fold(List(1, 2, 3)))
      println(Foldable[List].foldLeft(List(1, 2, 3), 1)((acc: Int, elem: Int) => acc * elem))

      case class Product(price: Double)
      println(Foldable[List].foldMap(List(Product(1.2), Product(2.4)))(p => p.price))

      println(List(Product(1.2), Product(2.4)).foldMap(_.price))
    }


    // ---
    println()

    {
      import cats.implicits._

      val f = Functor[Try] compose Functor[List] compose Functor[Option]

      val r = f.map(Success(List(Some(10), Some(11), None, None, Some(2))))(_ + 1)

      println(r)
    }

    // ---
    println()

    {

      import ApplicativeListImplicits._

      val r = Applicative[List].map2(List(1, 2, 3), List("a", "b", "c"))((num, char) => num.toString + char)
      println(r)


      val r2 = Applicative[List].unit(1)
      println(r2)


      val r3 = Applicative[List].map(List(2, 2, 3))(x => x + 1)
      println(r3)


      val r4 = Applicative[List].product(List(1, 2, 3), List("a", "b", "c"))
      println(r4)


      val lift: List[Int] => List[Option[Int]] = Applicative[List].lift((x: Int) => if (x > 0) Some(x) else None)
      val r5 = lift(List(1, 2, 3, 4))
      println(r5)


      val r6: List[List[Option[Int]]] = Applicative[List].traverse(List(1, 2, 3, 4))((x: Int) => List(Option(x)))
      println(r6)


      val add3 = (x: Int, y: Int, z: Int) => x + y + z
      println(add3(1, 2, 3))
      println(add3.curried.apply(1))
      println(add3.curried.apply(1).apply(2).apply(3))

      val curriedAdd = curried((x: Int, y: Int) => x + y)
      println(curriedAdd.apply(1).apply(2))

    }

    // ---
    println()

    {

      import scala.concurrent.ExecutionContext.Implicits.global
      import cats.implicits._

      case class Book(title: String)
      case class Album(name: String)
      case class Game(ranking: Int)

      case class Charts(books: Seq[Book], albums: Seq[Album], games: Seq[Game])

      def topBooks(): Future[Seq[Book]] = Future(Seq(Book("title1"), Book("title2")))

      def topAlbums(): Future[Seq[Album]] = Future(Seq(Album("album1"), Album("album2")))

      def topGames(): Future[Seq[Game]] = Future(Seq(Game(1), Game(2)))


      val r: Future[Charts]
      = cats.Applicative[Future].map3(topBooks(), topAlbums(), topGames())((book, album, game) => Charts(book, album, game))
      println(Await.ready(r, Duration.Inf))


      import cats.implicits._
      val r2: Future[Charts] = (topBooks() |@| topAlbums() |@| topGames()).map(Charts(_, _, _))
      println(Await.ready(r2, Duration.Inf))


      val r3 = (topBooks(), topAlbums(), topGames()).mapN(Charts)
      println(Await.ready(r3, Duration.Inf))

    }


    // ---
    println()

    {
      import scala.language.higherKinds

      case class Account(name: String, email: String)

      class AccountsRepository[F[_]] {

        def getName(id: String)(implicit M: Monad[F]): F[String] = {
          M.pure("some-name")
        }

        def getEmail(id: String)(implicit M: Monad[F]): F[String] = {
          M.pure("some-email")
        }

        def getAccount(id: String)(implicit M: Monad[F]): F[Account] = {
          M.map2(getName(id), getEmail(id))((name, email) => Account(name, email))
        }

      }

      import scala.concurrent.ExecutionContext.Implicits._
      import cats.instances.future._
      import cats.instances.try_._

      println(Await.result(new AccountsRepository[Future].getAccount("1"), Duration.Inf))
      println(new AccountsRepository[Try].getAccount("1"))

    }


    // ---
    println()

    {

      import cats.implicits._

      val r = for {
        i <- OptionT[List, Int](List(Option(1), Option(2)))
        j <- OptionT[List, Int](List(Option(3), Option(4)))
      } yield i + j

      println(r)

    }


    // ---
    println()

    {
      // Note: effectful functions, monadic compositions with kleisli.


      val k1 = Kleisli[Option, Int, String](x => Option(x.toString))
      val k2 = Kleisli[Option, String, Int](x => Option(x.toInt))

      val r = for {
        x1 <- k1(1)
        x2 <- k2(x1)
      } yield x2

      println(r)
    }


    // ---
    println()

    {

      import scala.concurrent.ExecutionContext.Implicits._


      sealed trait RequestType
      case object Get extends RequestType
      case object Post extends RequestType

      sealed trait HttpStatus
      case object Ok extends HttpStatus
      case object NotFound extends HttpStatus

      case class Request(requestType: RequestType, path: String)
      case class Response(httpStatus: HttpStatus)

      trait DecodeFailure

      type Service[A, B] = Kleisli[Future, A, B] // A => Future[B]

      type HttpService = Service[Request, Response] // Request => Future[Response]

      type DecodeResult[T] = EitherT[Future, DecodeFailure, T] // Future[Either[DecodeFailure, T]]

      object Service {
        def lift[A, B](f: A => Future[B]): Service[A, B] = Kleisli(f) // For DSL purposes
      }

      object HttpService {
        def apply(f: PartialFunction[Request, Response]): HttpService = {
          Service.lift(liftToAsync(f))
        }

        def liftToAsync[A, B](f: A => B): A => Future[B] = {
          a: A => Future(f(a))
        }
      }

      val httpService: HttpService = HttpService {
        case r1 @Request(Get, "/") => Response(Ok)
        case r2 @Request(Post, "/") => Response(NotFound)
      }


    }


  } //main.


  // ---

  trait Semigroup[A] {
    def combine(a1: A, a2: A): A
  }

  // ---

  case class ComplexNumber(x: Double, i: Double)

  object ComplexNumber {
    implicit val semigroup: Semigroup[ComplexNumber] = new Semigroup[ComplexNumber] {
      override def combine(a1: ComplexNumber, a2: ComplexNumber): ComplexNumber = {
        ComplexNumber(a1.x + a2.x, a1.i + a2.i)
      }
    }
  }

  object SuperAdder {
    def add[A](a1: A, a2: A)(implicit instance: Semigroup[A]): A = {
      instance.combine(a1, a2)
    }
  }

  // ---

  import scala.language.higherKinds

  trait SemigroupK[F[_]] {
    def combineK[A](fa1: F[A], fa2: F[A]): F[A]
  }

  // ---

  object OptionImplicits {
    implicit val optionSemigroupK: SemigroupK[Option] = new SemigroupK[Option] {
      override def combineK[A](fa1: Option[A], fa2: Option[A]): Option[A] = {
        fa1 orElse fa2
      }
    }
  }

  object ListImplicits {
    implicit val listSemigroupK: SemigroupK[List] = new SemigroupK[List] {
      override def combineK[A](fa1: List[A], fa2: List[A]): List[A] = fa1 ++ fa2
    }
  }


  // ---

  trait Applicative[F[_]] {

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit())((a, _) => f(a))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((a, b) => (a, b))

    def lift[A, B](f: A => B): F[A] => F[B] = { // lift == pimp my api
      fa: F[A] => map(fa)(f)
    }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
      as.foldRight(unit(List[B]()))((elem, acc) => map2(f(elem), acc)((l1, l2) => l1 +: l2))
    }

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
      map2(fab, fa)(_ (_))
    }
  }

  def curried[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  object ApplicativeListImplicits {
    implicit val listApplicative: Applicative[List] = new Applicative[List] {

      override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = {
        for {
          aElem <- fa
          bElem <- fb
          computedElem = f(aElem, bElem)
        } yield computedElem
      }

      override def unit[A](a: => A): List[A] = List(a)
    }
  }

  object Applicative {
    def apply[F[_]](implicit instance: Applicative[F]): Applicative[F] = {
      instance
    }
  }


  // ---

}
