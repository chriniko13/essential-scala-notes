package com.chriniko.essential.scala.chapter4_half

object test {


  def main(args: Array[String]): Unit = {

    // Structural Recursion

    // using polymorphism
    println(Lion().dinner())
    println(Tiger().dinner())
    println(Panther().dinner())
    println(Cat("hamster").dinner())


    println()


    // using pattern matching
    println(Feline.dinner(Lion()))
    println(Feline.dinner(Tiger()))
    println(Feline.dinner(Panther()))
    println(Feline.dinner(Cat("hamster")))

  }

}

// ---

sealed trait Feline {
  def dinner(): Food

  def dinner2(): Food = {
    this match {
      case _: Lion => Antelope
      case _: Tiger => TigerFood
      case _: Panther => Licorice
      case Cat(v) => CatFood(v)
    }
  }
}

final case class Lion() extends Feline {
  override def dinner(): Food = Antelope
}

final case class Tiger() extends Feline {
  override def dinner(): Food = TigerFood
}

final case class Panther() extends Feline {
  override def dinner(): Food = Licorice
}

final case class Cat(favouriteFood: String) extends Feline {
  override def dinner(): Food = CatFood(favouriteFood)
}


sealed trait Food
case object Antelope extends Food
case object TigerFood extends Food
case object Licorice extends Food
final case class CatFood(food:String) extends Food


object Feline {

  def dinner(f: Feline): Food = {
    f match {
      case _: Lion => Antelope
      case _: Tiger => TigerFood
      case _: Panther => Licorice
      case Cat(v) => CatFood(v)
    }
  }

}