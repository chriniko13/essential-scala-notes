package com.chriniko.essential.scala.chapter_5_variance

object test {

  def main(args: Array[String]): Unit = {

    // ---
    println


    println(Full(1))
    println(Empty)

    // ---
    println

    println(Box(1).set(2).v)

    // ---
    println

    println(Addition(Number(1), Number(2)).eval)
    assert(Addition(Number(1), Number(2)).eval == Success(3))

    println(SquareRoot(Number(-1)).eval)
    assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))

    println(Division(Number(4), Number(0)).eval)
    assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

    println(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval)
    assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval == Success(2.0))


    println

    println(Addition(Number(1), Number(2)).evalWithLift)
    assert(Addition(Number(1), Number(2)).evalWithLift == Success(3))

    println(SquareRoot(Number(-1)).evalWithLift)
    assert(SquareRoot(Number(-1)).evalWithLift == Failure("Square root of negative number"))

    println(Division(Number(4), Number(0)).evalWithLift)
    assert(Division(Number(4), Number(0)).evalWithLift == Failure("Division by zero"))

    println(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).evalWithLift)
    assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).evalWithLift == Success(2.0))

  }

}

// ---
case class Box[+A](v: A) {

  def map[B](f: Function1[A, B]): Box[B] = {
    Box(f(v))
  }

  def set[AA >: A](a: AA): Box[AA] = Box(a)

}

// ---
sealed trait Maybe[+A] {

  def map[B](f: A => B): Maybe[B] = {
    this match {
      case Full(v) => Full(f(v))
      case Empty => Empty
    }
  }

}

final case class Full[A](v: A) extends Maybe[A]

case object Empty extends Maybe[Nothing]


// ---
sealed trait Sum[+A, +B] {

  def map[C](f: B => C): Sum[A, C] = {
    this match {
      case Success(v) => Success(f(v))
      case Failure(v) => Failure(v)
    }
  }

  def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] = {
    this match {
      case Success(v) => f(v)
      case Failure(v) => Failure(v)
    }
  }

}

final case class Failure[A](a: A) extends Sum[A, Nothing]

final case class Success[B](b: B) extends Sum[Nothing, B]

// --- Type Bounds (>: , <:)

case class Visitor()

case class WebAnalytics[A <: Visitor](visitor: A, pageViews: Int, searchTerms: List[String], isOrganic: Boolean)

// ---

sealed trait Expression {

  def eval: Sum[String, Double] = {
    this match {

      case Addition(l, r) => l.eval.flatMap(x => r.eval.map(z => x + z))

      case Subtraction(l, r) => l.eval.flatMap(x => r.eval.map(z => x - z))

      case Division(l, r) => r match {
        case Number(v) if v == 0 => Failure("Division by zero")
        case _ => l.eval.flatMap(x => r.eval.map(z => x / z))
      }

      case SquareRoot(e) => e match {
        case Number(v) => if (v > 0) Success(Math.sqrt(v)) else Failure("Square root of negative number")
        case e@_ => e.eval
      }

      case Number(v) => Success(v)
    }
  }

  def evalWithLift: Sum[String, Double] = {

    this match {

      case Addition(l, r) => lift2(l, r, (x, y) => Success(x + y))

      case Subtraction(l, r) => lift2(l, r, (x, y) => Success(x - y))

      case Division(l, r) => lift2(l, r, (x, y) => if (y == 0) Failure("Division by zero") else Success(x / y))

      case SquareRoot(e) =>
        e.evalWithLift.flatMap(value =>
          if (value < 0) Failure("Square root of negative number") else Success(Math.sqrt(value))
        )

      case Number(v) => Success(v)
    }

  }

  // lift args and put them in a context (wrapper)
  def lift2(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]): Sum[String, Double] = {
    l.eval.flatMap(left => r.eval.flatMap(right => f(left, right)))
  }
}

final case class Addition(l: Expression, r: Expression) extends Expression

final case class Subtraction(l: Expression, r: Expression) extends Expression

final case class Division(l: Expression, r: Expression) extends Expression

final case class SquareRoot(value: Expression) extends Expression

final case class Number(value: Double) extends Expression


