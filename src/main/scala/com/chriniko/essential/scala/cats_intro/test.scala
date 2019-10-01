package com.chriniko.essential.scala.cats_intro

object test {

  def main(args: Array[String]): Unit = {

    // ---
    println()

    {

      import PrintableInstances._

      val r: String = Printable("chriniko")
      println(r)

      val r2 = "chriniko".print
      println(r2)
    }


    // ---
    println()
  }

  // ---


  // TYPE CLASS PATTERN

  // Note: type class
  trait Printable[T] {
    def format(value: T) : String
  }

  // Note: type class instances
  object PrintableInstances {

    implicit val intInstance: Printable[Int] = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

    implicit val stringInstance: Printable[String] = new Printable[String] {
      override def format(value: String): String = s"--- $value ---"
    }

  }

  // Note: type class interface
  object Printable {
    def apply[T](value: T)(implicit instance: Printable[T]): String = {
      instance format value
    }
  }

  // Note: type class - enrichment
  implicit class PrintableExtensions[T](t: T) {
    def print(implicit instance: Printable[T]): String = {
      instance.format(t)
    }
  }



}
