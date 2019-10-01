package com.chriniko.essential.scala.monad_2

object test {


  def main(args: Array[String]): Unit = {


    // ---
    println

    println(Computation.compute(Some(new Foo())))
    println(Computation.compute(None))

    println(Computation.compute2(Some(new Foo())))
    println(Computation.compute2(None))
  }


}


// ---

trait Monad[A] {

  def map[B](f: A => B) : Monad[B]

  def flatMap[B](f: A => Monad[B]): Monad[B]

}


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]
}


case class Some[A](a: A) extends Option[A] {

  override def map[B](f: A => B): Option[B] = Some(f(a))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(a)
}

case object None extends Option[Nothing] {

  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
}

// ---
class Foo {
  def bar: Option[Bar] = {
    Some(new Bar())
  }
}

class Bar {
  def baz: Option[Baz] = {
    Some(new Baz())
  }
}

class Baz {
  def compute: Int = 17
}

object Computation {

  def compute(maybeFoo: Option[Foo]): Option[Int] = {
    maybeFoo
      .flatMap(_.bar)
      .flatMap(_.baz)
      .map(_.compute)
  }

  def compute2(maybeFoo: Option[Foo]): Option[Int] = {
    for {
      foo <- maybeFoo
      bar <- foo.bar
      baz <- bar.baz
    } yield baz.compute
  }

}

// ---
sealed trait Validation[A, B] {

  def map[C](f: B => C): Validation[A, C]

  def flatMap[C](f: B => Validation[A, C]): Validation[A, C]

  def liftFail[D](f: A => D): Validation[D, B] // unrelated to monads!
}

// Note: right based

final case class Success[A, B](b: B) extends Validation[A, B] {

  override def map[C](f: B => C): Validation[A, C] = Success(f(b))

  override def flatMap[C](f: B => Validation[A, C]): Validation[A, C] = f(b)

  override def liftFail[D](f: A => D): Validation[D, B] = Success(b)
}

final case class Failure[A, B](a: A) extends Validation[A, B] {

  override def map[C](f: B => C): Validation[A, C] = Failure(a)

  override def flatMap[C](f: B => Validation[A, C]): Validation[A, C] = Failure(a)

  override def liftFail[D](f: A => D): Validation[D, B] = Failure(f(a))
}


case class ComputeError(str: String)

// ---
// State and Undo Monad
// Promise Monad
// Transaction Monad