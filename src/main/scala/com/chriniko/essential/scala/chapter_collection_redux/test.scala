package com.chriniko.essential.scala.chapter_collection_redux

object test {

  def main(args: Array[String]): Unit = {

    // ---
    println()

    val r = Seq(1, 2, 3).map(_ + 1).map(_ * 2).map(_ + 3)
    println(r)


    val r2 = Seq(1, 2, 3).view.map(_ + 1).map(_ * 2).map(_ + 3)
    println(r2)
    println(r2.force)

    // ---
    println()

    val animals = Seq("cat", "dog", "penguin")

    val animals2 = "mouse" +: animals :+ "tyrannosaurus"

    println(animals)
    println(animals2)

    val mAnimals = scala.collection.mutable.Seq("cat", "dog", "penguin")
    println(mAnimals)
    mAnimals(0) = "tiger"
    println(mAnimals)

  }

}
