package com.chriniko.essential.scala.chapter7

import java.util.Date

import scala.annotation.tailrec

object test {

  def main(args: Array[String]): Unit = {


    // ---
    println
    val minOrdering = Ordering.fromLessThan[Int](_ < _)
    val maxOrdering = Ordering.fromLessThan[Int](_ > _)

    println(List(3, 4, 2).sorted(minOrdering))
    println(List(3, 4, 2).sorted(maxOrdering))

    //implicit val ordering: Ordering[Int] = Ordering.fromLessThan[Int](_ > _)
    //println(List(3, 4, 2).sorted)


    // ---
    println

    val absOrdering = Ordering.fromLessThan[Int](Math.abs(_) < Math.abs(_))
    assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))


    implicit val absOrderingImpl: Ordering[Int] = Ordering.fromLessThan[Int](Math.abs(_) < Math.abs(_))
    assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))


    // ---
    println

    // Note: local scope implicit
    implicit val rationalOrdering: Ordering[Rational]
    = Ordering.fromLessThan[Rational]((o1, o2) =>
      (o1.numerator.toDouble / o1.denominator.toDouble) > (o2.numerator.toDouble / o2.denominator.toDouble))

    println(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted)

    // Note: if you want to uncomment it, uncomment also implicit from local scope
    //    assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted
    //      == List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))


    assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted
      ==
      List(Rational(3, 4), Rational(1, 2), Rational(1, 3)))


    // ---
    println


    println(collatzConjecture(1))
    println(collatzConjecture(9))
    println(collatzConjecture(123))
    println(collatzConjecture(97))
    println(collatzConjecture(871))
    println(collatzConjecture(6171))
    println(collatzConjecture(77031))
    println(collatzConjecture(837799))

    println(collatzConjectureUpperBound(9))


    // ---
    println


    trait HtmlWriteable {
      def toHtml: String
    }

    final case class Person(name: String, email: String) extends HtmlWriteable {
      override def toHtml: String = s"<span>$name $email</span>"

      def stringify(implicit w: HtmlWriter[Person]): String = {
        w.write(this)
      }

      def equal(p: Person)(implicit e: Equal[Person]): Boolean = {
        e.compare(this, p)
      }
    }

    println(Person("chriniko", "chriniko@mail.gr").toHtml)


    // ---
    println()

    // adapter class
    trait HtmlWriter[A] {
      def write(in: A): String
    }

    implicit object PersonWriter extends HtmlWriter[Person] {
      override def write(in: Person): String = s"<span>${in.name} ${in.email}</span>"
    }
    println(Person("chriniko", "chriniko@mail.gr").stringify)


    object DateWriter extends HtmlWriter[Date] {
      override def write(in: Date): String = in.toString
    }
    println(DateWriter.write(new Date()))


    object ObfuscatedPersonWriter extends HtmlWriter[Person] {
      def write(person: Person): String
      = s"<span>${person.name} (${person.email.replaceAll("@", " at ")})</ span>"}
    println(ObfuscatedPersonWriter.write(Person("chriniko", "chriniko@mail.gr")))

    println
    println(Person("f", "e") == Person("f", "e"))
    println(Person("f", "e") eq Person("f", "e"))


    // ---
    println

    trait Equal[A] {
      def compare(o1: A, o2: A) : Boolean
    }

    object PersonEmailEqual extends Equal[Person] {
      override def compare(o1: Person, o2: Person): Boolean = o1.email == o2.email
    }
    object PersonNameAndEmailEqual extends Equal[Person] {
      override def compare(o1: Person, o2: Person): Boolean = o1.email == o2.email && o1.name == o2.name
    }
    println(Person("s", "e").equal(Person("f", "e"))(PersonEmailEqual))
    println(Person("f", "e").equal(Person("f", "e"))(PersonNameAndEmailEqual))


    implicit object PersonAlwaysTrueEqual extends Equal[Person] {
      override def compare(o1: Person, o2: Person): Boolean = true
    }
    println(Person("f", "e").equal(Person("f1", "e1")))


    // Type Class Interface Pattern.
    object Equal {
      def apply[A](implicit e: Equal[A]): Equal[A] = {
        e
      }
    }
    println(Equal[Person].compare(Person("f", "e"), Person("f", "e")))
  }

  // ---

  final case class Rational(numerator: Int, denominator: Int)

  object Rational {

    implicit val rationalOrdering: Ordering[Rational]
    = Ordering.fromLessThan[Rational]((o1, o2) =>
      (o1.numerator.toDouble / o1.denominator.toDouble) < (o2.numerator.toDouble / o2.denominator.toDouble))

  }

  object RationalLessThanOrdering {
    implicit val ordering: Ordering[Rational] = Ordering.fromLessThan[Rational]((x, y) =>
      (x.numerator.toDouble / x.denominator.toDouble) <
        (y.numerator.toDouble / y.denominator.toDouble)
    )
  }

  object RationalGreaterThanOrdering {
    implicit val ordering: Ordering[Rational] = Ordering.fromLessThan[Rational]((x, y) =>
      (x.numerator.toDouble / x.denominator.toDouble) >
        (y.numerator.toDouble / y.denominator.toDouble)
    )
  }

  def collatzConjecture(number: Long): Long = {
    @tailrec
    def helper(n: Long, steps: Long): Long = {
      if (n == 1) {
        steps
      } else if (n % 2 == 0) {
        helper(n / 2, steps + 1)
      } else {
        helper(3 * n + 1, steps + 1)
      }
    }
    helper(number, 0)
  }

  def collatzConjectureUpperBound(upperBound: Long): Seq[(Long /*steps*/ , Long /*number*/ )] = {
    (1L to upperBound).map(num => (collatzConjecture(num), num))
  }

}

// ---

final case class Order(units: Int, unitPrice: Double) {
  val totalPrice: Double = units * unitPrice
}

object Order {
  //default one.
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan[Order](_.totalPrice < _.totalPrice)
}

object OrderSortByUnits {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan[Order](_.units < _.units)
}

object OrderSortByUnitPrice {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan[Order](_.unitPrice < _.unitPrice)
}

// ---


