package com.chriniko.essential.scala.chapter3

object test {


  def main(args: Array[String]): Unit = {

    case class Person(firstname:String, lastname:String)

    val p1 = Person("first", "last")
    val p2 = Person("first", "last")


    println(p1 equals p2)
    println(p1 == p2)
    println(p1 eq p2) // reference equality


    // ---


    case object Citizen {
      def hello = "Hello World"
    }

    println(Citizen.hello)

    // ---

    p1 match {
      case Person(f, l) if f == "first" => println(s"$f $l")
    }

  }


}



