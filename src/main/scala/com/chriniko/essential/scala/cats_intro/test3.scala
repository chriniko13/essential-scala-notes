package com.chriniko.essential.scala.cats_intro

import cats.Show

object test3 {


  def main(args: Array[String]): Unit = {

    // ---
    println()

    {
      import cats.instances.int.catsStdShowForInt
      import cats.instances.string.catsStdShowForString

      val showInt = Show[Int]
      val showString = Show[String]

      println(showInt.show(123))
      println(showString.show("abc"))


      import cats.syntax.show._
      println(123.show)
      println("abc".show)

    }
  }


}
