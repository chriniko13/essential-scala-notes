package com.chriniko.essential.scala.cats_intro

object test2 {


  def main(args: Array[String]): Unit = {

    // ---
    println()

    {
      import PrintableInstances._

      val r = Cat("Kitten", 5, "brown").print
      println(r)

    }
  }


  // ---
  case class Cat(name:String, age:Int, color:String)

  // ---
  trait Printable[T] {
    def format(value: T): String
  }

  object PrintableInstances {

    implicit val catInstance: Printable[Cat] = new Printable[Cat] {
      override def format(value: Cat): String = s"${value.name} is a ${value.age} years old, with ${value.color} color"
    }
  }

  implicit class PrintableExtensions[T](t: T) {
    def print(implicit instance: Printable[T]): String = {
      instance.format(t)
    }
  }
}
