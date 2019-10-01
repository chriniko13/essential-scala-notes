package com.chriniko.essential.scala.chapter7_part_two

import scala.language.{implicitConversions, postfixOps}

object test {

  def main(args: Array[String]): Unit = {

    // ---
    val p1 = Person("f", "e")
    val p2 = Person("w", "d")


    implicit object PersonNameEquality extends Equal[Person] {
      override def compare(o1: Person, o2: Person): Boolean = o1.name == o2.name
    }
    val result = Equal[Person].compare(p1, p2)
    println(result)


    implicit class EqualOps[T](t: T) {
      def comparison(other: T)(implicit e: Equal[T]): Boolean = {
        e.compare(t, other)
      }
    }

    println(Person("w", "h").comparison(Person("n", "h")))

    implicit object StringEqual extends Equal[String] {
      override def compare(o1: String, o2: String): Boolean = o1 == o2
    }
    println("".comparison(""))


    // ---
    println()


    // enriched interfaces / enrichments
    implicit class MyEnrichmentForString(str: String) {
      def meow(): Unit = {
        println("meow")
      }
    }

    "nikos".meow()


    // ---
    println()

    def numberOfVowels(str: String): Int = {
      str.filter(x => Seq('a', 'e', 'i', 'o', 'u').contains(x)).foldLeft(0)((acc, _) => acc + 1)
    }

    println(numberOfVowels("the quick brown fox"))


    implicit class ExtraStringMethods(str: String) {
      def numberOfVowels(): Int = {
        str.filter(x => Seq('a', 'e', 'i', 'o', 'u').contains(x)).foldLeft(0)((acc, _) => acc + 1)
      }
    }

    println("the quick brown fox".numberOfVowels())


    // ---
    println()


    {
      import IntImplicits._
      2.yeah()
      3.yeah()
      -1 yeah

      3.times(i => println(s"Look - it's the number $i!"))
    }



    // ---
    println()

    {
      implicit object StringEqual extends Equal[String] {
        override def compare(o1: String, o2: String): Boolean = o1.toLowerCase() == o2.toLowerCase
      }

      implicit class StringExtensions(str: String) {
        def ===(other: String)(implicit typeInstance: Equal[String]): Boolean = {
          typeInstance.compare(str, other)
        }
      }

      println("abcd" === "ABCD")

      println(implicitly[Equal[String]] == StringEqual)


      // ---
      println()

      case class A(a: String)
      case class B(b: String)

      implicit def makeItFit(a: A): B = B(a.a)

      val b : B = A("here")
      println(b)



      // ---
      println()

      {

        object ExtraIntMethodsWithImplicitConversions {
          class IntMethods(i: Int) {
            def yeah(): Unit = {
              times(_ => println("oh yeah!"))
            }

            def times(f: Int => Unit): Unit = {
              (0 until i).foreach(f)
            }
          }

          implicit def intToIntOps(value: Int): IntMethods = new IntMethods(value)
        }

        import ExtraIntMethodsWithImplicitConversions._
        1.yeah()
      }


    }
  }

}

// ---
case class Person(name: String, email: String)

trait Equal[A] {
  def compare(o1: A, o2: A): Boolean
}

object Equal {
  def apply[A](implicit typeClassInstance: Equal[A]): Equal[A] = {
    typeClassInstance
  }
}

object Eq {
  def run[A](o1: A, o2: A)(implicit p: Equal[A]): Boolean = {
    p.compare(o1, o2)
  }
}

object EmailImplicit {

  implicit object EmailEqual extends Equal[Person] {
    def compare(v1: Person, v2: Person): Boolean =
      v1.email == v2.email
  }

}

object NameEmailImplicit {

  implicit object NameEmailEqual extends Equal[Person] {
    def compare(v1: Person, v2: Person): Boolean =
      v1.email == v2.email && v1.name == v2.name
  }

}


// ---
object IntImplicits {

  implicit class ExtraIntMethods(i: Int) {

    def yeah(): Unit = {
      times(_ => println("oh yeah!"))
    }

    def times(f: Int => Unit): Unit = {
      (0 until i).foreach(f)
    }
  }

}
