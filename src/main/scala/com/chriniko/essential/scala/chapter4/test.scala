package com.chriniko.essential.scala.chapter4

import java.util.Date

object test {

  def main(args: Array[String]): Unit = {

    val anonymous = Anonymous("1")
    Thread.sleep(1000)
    val user = User("2", email = "2@mail.com")


    println(anonymous)
    println(user)

    val result = Visitor.older(anonymous, user)

    println(result == anonymous)
    assert(result == anonymous)

    println(result eq anonymous)
    assert(result eq anonymous)

    // ---
    println(Lion("yellow", 5))


    // ---
    println(divide(1, 2))
    println(divide(1, 0))

    // ---
    val aB = B()
    val aC = C()

    println(aB.f)
    println(aC.f)


    // ---
    println(Red.next)
    println(Red.next.next)
    println(Red.next.next.next)
    println(Red.next.next.next.next)

    // ---
    println

    println(Calculator.+(Success(1), 1) == Success(2))
    assert(Calculator.+(Success(1), 1) == Success(2))

    println(Calculator.-(Success(1), 1) == Success(0))
    assert(Calculator.-(Success(1), 1) == Success(0))

    println(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))
    assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

    println

    println(Calculator./(Success(4), 2))
    assert(Calculator./(Success(4), 2) == Success(2))

    println(Calculator./(Success(4), 0))
    assert(Calculator./(Success(4), 0) == Failure("Division by zero"))

    println(Calculator./(Failure("Badness"), 0))
    assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))
  }

}

// ---

trait Visitor {
  def id: String

  def createdAt: Date

  def age: Long = new Date().getTime - createdAt.getTime

}

object Visitor {
  def older(v1: Visitor, v2: Visitor): Visitor = {
    if (v1.createdAt.before(v2.createdAt)) v1 else v2
  }
}

case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

case class User(id: String, createdAt: Date = new Date(), email: String) extends Visitor


// ---

trait Feline {
  def colour: String

  def sound: String
}

trait BigCat extends Feline {
  override def sound: String = "roar"
}

case class Lion(colour: String, maneSize: Int) extends BigCat

case class Tiger(colour: String) extends BigCat

case class Panther(colour: String) extends BigCat

case class Cat(colour: String, food: String) extends Feline {
  val sound = "meow"
}

// ---

object divide {
  def apply(n1: Int, n2: Int): DivisionResult = {
    try {
      Finite(n1 / n2)
    } catch {
      case _: ArithmeticException => Infinite
    }
  }
}

sealed trait DivisionResult

final case class Finite(result: Float) extends DivisionResult

case object Infinite extends DivisionResult

// ---

sealed trait TrafficLight {
  def next: TrafficLight = {
    this match {
      case Red => Green
      case Green => Yellow
      case Yellow => Red
    }
  }
}

case object Red extends TrafficLight

case object Yellow extends TrafficLight

case object Green extends TrafficLight

// ---

sealed trait Calculation

final case class Success(v: Int) extends Calculation

final case class Failure(m: String) extends Calculation

object Calculator {

  def +(c: Calculation, i: Int) : Calculation = {
    c match {
      case f:Failure => f
      case s:Success => Success(s.v + i)
    }
  }

  def -(c: Calculation, i: Int) : Calculation = {
    c match {
      case f:Failure => f
      case s:Success => Success(s.v - i)
    }
  }

  def /(c: Calculation, i: Int) : Calculation = {
    c match {
      case f:Failure => f
      case s:Success => i match {
        case 0 => Failure("Division by zero")
        case _ => Success(s.v / i)
      }
    }
  }

}




// ---
final case class BottledWater(size: Int, source: WaterSource, carbonated: Boolean)

sealed trait WaterSource

case object Well extends WaterSource

case object Spring extends WaterSource

case object Tap extends WaterSource

// ---

sealed trait A {
  def f = "A"
}

final case class B() extends A {
  override def f: String = "B"
}

final case class C() extends A {
  override def f: String = "C"
}