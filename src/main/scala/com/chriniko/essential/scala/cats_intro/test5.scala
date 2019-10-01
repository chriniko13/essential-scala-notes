package com.chriniko.essential.scala.cats_intro

import cats.data.EitherT
import cats.{Applicative, Functor, Monad, Traverse}
import cats.kernel.Monoid

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

object test5 {

  def main(args: Array[String]): Unit = {

    // ---
    println()

    {
      import cats.instances.string._
      val r = Monoid[String].combine("a", "b")
      println(r)

      import cats.syntax.monoid._
      val r2 = "a".combine("b")
      println(r2)
    }


    // ---
    println()

    {

      import cats.instances.int._
      import cats.instances.option._
      import cats.syntax.semigroup._

      val e = Monoid[Option[Int]].empty
      println(e)

      val r = Option(1) |+| None
      println(r)

      val r2 = Option(1).combine(Option(2))
      println(r2)

      val r3 = Monoid.combine(Option(1), Option(2))
      println(r3)
    }


    // ---
    println()

    {
      import cats.instances.option._

      val f: Functor[Option] = Functor[Option]

      val r: Option[Int] = f.map(Option(1))(_ + 10)
      println(r)

      val l: Option[Int] => Option[Int] = f.lift((x: Int) => x + 1)
      val r2: Option[Int] = l(Option(2))
      println(r2)
      println(l(None))


      case class Box[T](t: T)

      implicit val animalFunctor: Functor[Box] = new Functor[Box] {
        override def map[A, B](fa: Box[A])(f: A => B): Box[B] = {
          Box(f(fa.t))
        }
      }

      val boxFunctor: Functor[Box] = Functor[Box]
      val boxResult: Box[Int] = boxFunctor.map(Box(1))(_ + 1)
      println(boxResult)

    }


    // ---
    println()

    {

      import cats.instances.string._
      import cats.instances.option._

      import cats.syntax.flatMap._
      import cats.syntax.monad._
      import cats.syntax.applicative._
      import cats.data.OptionT._ //etc... EitherT, ReaderT, WriterT, StateT,...

      val monad: Monad[Option] = Monad[Option]

      val pure: Option[Int] = monad.pure(3)
      println(pure)

      println(monad.map(Some(3))(_ + 1))
      println(monad.flatMap(Some(20))(x => Some(x + 1)))

    }


    // ---
    println()

    {

      import scala.concurrent.ExecutionContext.Implicits._

      object Example {

        private def parseDouble(s: String): Either[String, Double] = {
          Try(s).map(_.toDouble).toEither.left.map(_.getMessage)
        }

        private def divide(a: Double, b: Double): Either[String, Double] = {
          b match {
            case 0 => Left("division by zero")
            case _ => Right(a / b)
          }
        }

        private def parseDoubleAsync(s: String): Future[Either[String, Double]] = {
          Future(parseDouble(s))
        }

        private def divideAsync(a: Double, b: Double): Future[Either[String, Double]] = {
          Future(divide(a, b))
        }

        def divisionProgram(input1: String, input2: String): Either[String, Double] = {
          for {
            num1 <- parseDouble(input1)
            num2 <- parseDouble(input2)
            res <- divide(num1, num2)
          } yield res
        }

        def divisionProgramAsync(input1: String, input2: String): Future[Either[String, Double]] = {

          parseDoubleAsync(input1)
            .flatMap(either1 =>
              parseDoubleAsync(input2).flatMap(either2 => {
                (either1, either2) match {
                  case (Right(a), Right(b)) => divideAsync(a, b)
                  case (Left(a), _) => Future(Left(a))
                  case (_, Left(b)) => Future(Left(b))
                }
              })
            )
        }

        def divisionProgramAsync2(input1: String, input2: String): EitherT[Future, String, Double] = {

          import cats.instances.future._

          for {
            num1 <- EitherT(parseDoubleAsync(input1))
            num2 <- EitherT(parseDoubleAsync(input2))
            res <- EitherT(divideAsync(num1, num2))
          } yield res
        }

      } // ExampleWithoutCats.

      println(Example.divisionProgram("10", "2"))
      println(Example.divisionProgram("a", "10"))
      println(Example.divisionProgram("10", "b"))
      println(Example.divisionProgram("1", "0"))

      println()

      println(Await.ready(Example.divisionProgramAsync("10", "2"), Duration.Inf))
      println(Await.ready(Example.divisionProgramAsync("a", "10"), Duration.Inf))
      println(Await.ready(Example.divisionProgramAsync("10", "b"), Duration.Inf))
      println(Await.ready(Example.divisionProgramAsync("1", "0"), Duration.Inf))

      println()

      println(Await.ready(Example.divisionProgramAsync2("10", "2").value, Duration.Inf))
      println(Await.ready(Example.divisionProgramAsync2("a", "10").value, Duration.Inf))
      println(Await.ready(Example.divisionProgramAsync2("10", "b").value, Duration.Inf))
      println(Await.ready(Example.divisionProgramAsync2("1", "0").value, Duration.Inf))

    }


    // ---
    println()

    {
      import scala.language.higherKinds

      trait M[F[_], A, B] {
        def map(fa: F[A])(f: A => B): F[B]

        def flatMap(fa: F[A])(f: A => F[B]): F[B]
      }

      implicit val defaultBox: M[Option, Int, Int] with Object = new M[Option, Int, Int] {
        override def map(fa: Option[Int])(f: Int => Int): Option[Int] = {
          fa match {
            case Some(value) => Some(f(value))
            case None => None
          }
        }

        override def flatMap(fa: Option[Int])(f: Int => Option[Int]): Option[Int] = {
          fa match {
            case Some(value) => f(value)
            case None => None
          }
        }
      }

      object M {
        def map(fa: Option[Int])(f: Int => Int)(implicit instance: M[Option, Int, Int]): Option[Int] = {
          instance.map(fa)(f)
        }

        def flatMap(fa: Option[Int])(f: Int => Option[Int])(implicit instance: M[Option, Int, Int]): Option[Int] = {
          instance.flatMap(fa)(f)
        }
      }

      println(M.map(Some(1))(_ + 1))
      println(M.flatMap(Some(1))(x => Some(x + 1)))

    }


    // ---
    println()

    {

    }

  } //main.

}
