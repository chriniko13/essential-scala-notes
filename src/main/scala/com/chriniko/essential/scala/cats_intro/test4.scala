package com.chriniko.essential.scala.cats_intro

import cats.Show

object test4 {

  def main(args: Array[String]): Unit = {

    // ---
    println()

    {
      import ShowCat._
      val showCat = Show[Cat]

      val r = showCat.show(Cat("Kitten", 2, "brown"))
      println(r)


      val r2 = Cat("Kitten", 2, "brown").show
      println(r2)

    }

  }

  // ---
  case class Cat(name:String, age:Int, color:String)

  object ShowCat {
    implicit val catInstance: Show[Cat] = new Show[Cat] {
      override def show(t: Cat): String = s"${t.name} is a ${t.age} years old, with ${t.color} color"
    }
  }

  implicit class CatExtensions(cat: Cat) {
    def show(implicit instance: Show[Cat]): String = {
      instance.show(cat)
    }
  }


}
