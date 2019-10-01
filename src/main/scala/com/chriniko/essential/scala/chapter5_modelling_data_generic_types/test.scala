package com.chriniko.essential.scala.chapter5_modelling_data_generic_types

object test {

  def main(args: Array[String]): Unit = {

    val pair = Pair[String, Int]("hi", 2)

    println(pair.v1)
    println(pair.v2)


    // ---

    println

    println(Tuple2("a", 1))
    println(("a", 1))

    val x: (Int, String, Boolean) = (1, "b", true)
    println(x._3)


    // ---

    println

    println(Failure[Int, String](1).v)
    println(Success[Int, String]("foo").v)

    val sum: Sum[Int, String] = Success("foo")

    sum match {
      case Failure(_) => println("left")
      case Success(_) => println("right")
    }

    // ---

    println()

    println(Full(1))
    println(Empty)

    println(Full(2).fold(0)(r => r * 2))
    println(Empty[Int]().fold(0)(r => r * 2))
    println(Empty[Int]().fold2(r => r * 2))

    // ---

    println()

    println(Success(1).fold(x => 2 * x))
    println(Failure[String, Int]("io error").fold(x => 2 * x))
    println(Success(1).fold(x => Right(2 * x)))


    // ---

    println

    val l = LLPair(1, LLPair(2, LLPair(3, End())))
    println(l)

    val l2 = l.map(_ + 10)
    println(l2)


    // ---
    println
    println(
      Full(3).flatMap(x => Full(x + 10).flatMap(y => Full(y + 1)))
    )

    // ---
    println

    val ll1 = LLPair(1, LLPair(2, LLPair(3, End())))
    val ll2 = LLPair(4, LLPair(5, LLPair(6, End())))
    val ll3 = LLPair(7, LLPair(8, LLPair(9, End())))
    val ll4 = LLPair(10, LLPair(11, LLPair(12, End())))

    println(ll1 + ll2)
    println(ll1 + End())
    println(End() + ll1)

    println(ll1 + (ll1, ll2, ll3, ll4))


    // ---
    println

    println(ll1.map(_ * 2))
    println(ll1.map(_ + 1))
    println(ll1.map(_ / 3))

    // ---
    println

    println(Full(1).map(_ + 10))
    println(Full(1).map2(_ + 10))

    // ---
    println

    val list = List(1, 2, 3)

    val negationF: Int => List[Int] = l => List(l, -l)

    println(list.map(negationF))
    println(list.flatMap(negationF))
    println(list.map(negationF).flatten)


    // ---
    println

    val list2: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))

    val fOdd: Maybe[Int] => Maybe[Int] = elem => elem match {
      case Full(v) => if (v % 2 != 0) Empty() else Full(v)
      case Empty() => Empty()
    }

    println(list2.map(fOdd))

    // ---
    println


    println(Success(1).map(_ + 1))
    println(Failure[String, Int]("no luck").map(_ + 1))
    println(Success[String, Int](1).flatMap(x => Success(x + 10)))

  }


}

// ---

case class Pair[A, B](v1: A, v2: B)


// ---

sealed trait Sum[A, B] {

  def fold[C](f: B => C): Sum[A, C] = {
    this match {
      case Failure(v) => Failure(v)
      case Success(v) => Success(f(v))
    }
  }

  def map[C](f: B => C): Sum[A, C] = {
    this match {
      case Success(v) => Success(f(v))
      case Failure(v) => Failure(v)
    }
  }

  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] = {
    this match {
      case Success(v) => f(v)
      case Failure(v) => Failure(v)
    }
  }

}

case class Failure[A, B](v: A) extends Sum[A, B]

case class Success[A, B](v: B) extends Sum[A, B]


// ---

sealed trait Maybe[A] {

  def fold[B](empty: B)(f: A => B): Maybe[B] = {
    this match {
      case Full(v) => Full(f(v))
      case Empty() => Full(empty)
    }
  }

  def fold2[B](f: A => B): Maybe[B] = {
    this match {
      case Full(v) => Full(f(v))
      case Empty() => Empty()
    }
  }

  def map[B](f: A => B): Maybe[B] = {
    this match {
      case Full(v) => Full(f(v))
      case Empty() => Empty()
    }
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = {
    this match {
      case Full(v) => f(v)
      case Empty() => Empty()
    }
  }

  def map2[B](f: A => B): Maybe[B] = {
    flatMap(f.andThen(x => Full(x)))
  }

}

final case class Full[A](v: A) extends Maybe[A]

final case class Empty[A]() extends Maybe[A]


// ---


sealed trait LinkedList[A] {

  def map[B](f: A => B): LinkedList[B] = {

    this match {
      case LLPair(head, tail) => LLPair(f(head), tail.map(f))
      case End() => End()
    }

  }

  def flatMap[B](f: A => LinkedList[B]): LinkedList[B] = {

    this match {
      case LLPair(head, tail) => f(head) + tail.flatMap(f)
      case End() => End()
    }

  }

  def +(other: LinkedList[A]): LinkedList[A] = {

    def helper(current: LinkedList[A], other: LinkedList[A], merged: Boolean): LinkedList[A] = {
      current match {
        case LLPair(head, tail) => LLPair(head, helper(tail, other, merged))
        case End() =>
          if (merged) {
            End()
          } else { // Note: proceed to other list.
            helper(other, other, merged = true)
          }
      }
    }

    helper(current = this, other = other, merged = false)
  }

  def +(others: LinkedList[A]*): LinkedList[A] = {
    others.fold(this)((acc, elem) => acc + elem)
  }

}

final case class LLPair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

final case class End[A]() extends LinkedList[A]