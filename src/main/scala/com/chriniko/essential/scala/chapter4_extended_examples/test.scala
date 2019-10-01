package com.chriniko.essential.scala.chapter4_extended_examples

object test {

  def main(args: Array[String]): Unit = {


    // (3 + 4) + (1 - 2)
    val e1 = Addition(
      Addition(Number(3), Number(4)),
      Subtraction(Number(1), Number(2))
    )
    println(e1.eval())
    assert(e1.eval() == Success(6))


    val e2 = Division(Addition(Number(3), Number(7)), Number(2))
    println(e2.eval())
    assert(e2.eval() == Success(5))


    val e3 = SquareRoot(Addition(Number(3), Number(7)))
    println(e3.eval())
    assert(e3.eval() == Success(3.1622776601683795))


    val e4 = SquareRoot(Number(-10))
    println(e4.eval())
    assert(e4.eval() == Failure("Square root of negative number"))


    val e5 = SquareRoot(Number(2))
    println(e5.eval())
    assert(e5.eval() == Success(1.4142135623730951))

    println


    assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Square root of negative number"))

    assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))

    assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

  }

}

// ---

sealed trait Expression {

  def eval(): Calculation = {
    this match {
      case Number(v) => Success(v)

      case Addition(l, r) => l.eval() match {
        case Success(v1) => r.eval() match {
          case Success(v2) => Success(v1 + v2)
          case f@Failure(msg) => f
        }
        case f@Failure(msg) => f
      }

      case Subtraction(l, r) => l.eval() match {
        case Success(v1) => r.eval() match {
          case Success(v2) => Success(v1 - v2)
          case f@Failure(_) => f
        }
        case f@Failure(_) => f
      }

      case Division(l, r) => l.eval() match {
        case Success(v1) => r.eval() match {
          case Success(v2) =>
            if (v2 == 0) Failure("Division by zero")
            else Success(v1 / v2)
          case f@Failure(_) => f
        }
        case f@Failure(_) => f
      }

      case SquareRoot(e) => e match {

        case Number(n) =>
          if (n < 0) Failure("Square root of negative number")
          else Success(Math.sqrt(n))

        case  expr: Expression =>
          expr.eval() match {
            case Success(v) => Success(Math.sqrt(v))
            case f@Failure(_) => f
          }
      }
    }
  }

}

final case class Addition(l: Expression, r: Expression) extends Expression

final case class Subtraction(l: Expression, r: Expression) extends Expression

final case class Number(v: Double) extends Expression

final case class Division(l: Expression, r: Expression) extends Expression

final case class SquareRoot(e: Expression) extends Expression

sealed trait Calculation

final case class Success(v: Double) extends Calculation
final case class Failure(msg: String) extends Calculation

// ---

