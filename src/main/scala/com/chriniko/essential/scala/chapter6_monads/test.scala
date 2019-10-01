package com.chriniko.essential.scala.chapter6_monads

import scala.collection.immutable.ListMap
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

object test {


  def main(args: Array[String]): Unit = {

    // ---
    println

    import scala.concurrent.ExecutionContext.Implicits._

    val r: Future[Int] = for {
      v1 <- Future(1)
      v2 <- Future(2)
    } yield v1 + v2

    Await.ready(r, Duration.Inf)

    println(r)


    // ---
    println


    val opt1 = Some(1)
    val opt2 = Some(2)
    val opt3 = Some(3)

    val optAll = for {
      v1 <- opt1
      v2 <- opt2
      v3 <- opt3
    } yield v1 + v2 + v3
    println(optAll)


    val seq1 = Seq(1)
    val seq2 = Seq(2)
    val seq3 = Seq(3)
    val seqAll = for {
      v1 <- seq1
      v2 <- seq2
      v3 <- seq3
    } yield v1 + v2 + v3
    println(seqAll)

    val try1 = Try(1)
    val try2 = Try(2)
    val try3 = Try(3)
    val tryAll = for {
      v1 <- try1
      v2 <- try2
      v3 <- try3
    } yield v1 + v2 + v3
    println(tryAll)


    // ---
    println


    println
    val filteredSeq: Seq[Int] = for {
      elem <- Seq(-1, -2, -3, -4, 1, 2, 3, 4, 5) if elem > 0
    } yield elem
    println(filteredSeq)


    println
    println(Seq(1, 2, 3).zip(Seq(4, 5, 6)).map(t => t._1 + t._2))

    println
    val z = for {
      x <- Seq(1, 2, 3)
      y <- Seq(4, 5, 6)
    } yield x + y
    println(z)


    println()
    val ex: Seq[Int] = for {
      x <- Seq(1, 2, 3).zip(Seq(1, 2, 3))
    } yield {
      val (one, two) = x
      one + two
    }
    println(ex)


    println()
    println(
      for {
        (a, b) <- Seq(1, 2, 3).zip(Seq(1, 2, 3))
      } yield a + b
    )

    println()
    println(
      for {
        a <- Seq(1, 2, 3)
        b = a * a
      } yield b
    )

    // ---
    println

    println
    println("2" -> 1)
    val example = Map("a" -> 1, "b" -> 2, "c" -> 3)
    println(example)

    println(example("a"))
    println(example.get("a"))
    println(example.contains("c"))


    println(example + ("d" -> 4))
    println(example - "a" + ("d" -> 4))



    // ---
    println()

    val mMap = scala.collection.mutable.Map("x" -> 10, "y" -> 20)
    println(mMap)

    mMap += "z" -> 30
    println(mMap)

    mMap("w") = 40
    println(mMap)


    // ---
    println

    val nMap = Map[String, Int]("a" -> 1) + ("b" -> 2) + ("c" -> 3) + ("d" -> 4) + ("e" -> 5)
    println(nMap)

    val lMap = ListMap("a" -> 1) + ("b" -> 2) + ("c" -> 3) + ("d" -> 4) + ("e" -> 5)
    println(lMap)


    println
    println(nMap.map[String](t => t._1 + "=" + t._2))
    println(nMap.map[String, Int](t => t._1 + "#" -> t._2 * 10))

    println(nMap.flatMap[String, Int](k => (1 to 3).map(x => (k._1 + x) -> (k._2 * x))))


    // ---
    println

    favoriteColor("Alice")
    favoriteColor("Bob")
    printColors()
    printColors2()


    println(lookup("Alice", ages))
    println(lookup("Nick", ages))

    println(lookup("Bob", favoriteColors))
    println(lookup("Nick", favoriteColors))

    println(lookup("Alice", favoriteLolcats))
    println(lookup("Nick", favoriteLolcats))


    println(ages.max)
    println
    println(findColorOfOldestPerson())
    println(findColorOfOldestPerson2())

    // ---
    println()

    println(union(Set(1, 2, 3), Set(4, 5, 6)))

    println
    val m1 = Map('a' -> 1, 'b' -> 2)
    val m2 = Map('a' -> 2, 'b' -> 4)
    println(union(m1, m2))

    println(union(m1, m2, (a: Int, b: Int) => a + b))

    val mN1 = Map('a' -> List(1), 'b' -> List(2))
    val mN2 = Map('a' -> List(3), 'b' -> List(4))

    println(union(mN1, mN2, (a: List[Int], b: List[Int]) => a ++ b))


    // ---
    println()

    println((1 to 10) mkString ",")
    println((1 until 10) mkString ",")
    println((10 until 1 by -1) mkString ",")

    for {i <- 1 to 10} println(i)

    println((1 to 10) ++ (20 to 30))

  }


  // ---

  val people: Set[String] = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")

  val ages: Map[String, Int] = Map(
    "Alice" -> 20,
    "Bob" -> 30,
    "Charlie" -> 50,
    "Derek" -> 40,
    "Edith" -> 10,
    "Fred" -> 60)

  val favoriteColors: Map[String, String] = Map(
    "Bob" -> "green",
    "Derek" -> "magenta",
    "Fred" -> "yellow")

  val favoriteLolcats: Map[String, String] = Map(
    "Alice" -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith" -> "Cloud Cat")

  def favoriteColor(personName: String): String = {
    favoriteColors.getOrElse(personName, "beige")
  }

  def printColors(): Unit = {
    favoriteColors.values.foreach(println)
  }

  def printColors2(): Unit = {
    for {
      person <- people
      favColor = favoriteColor(person)
    } println(s"$person favorite color is: $favColor")
  }

  def lookup[A](name: String, m: Map[String, A]): Option[A] = {
    m.get(name)
  }

  def findColorOfOldestPerson(): Option[String] = {
    favoriteColors.get(ages.max._1)
  }

  def findColorOfOldestPerson2(): Option[String] = {

    val maxAgePerson
    = people.foldLeft[(String, Int)]((people.head, ages.head._2))((acc, elem) => if (ages(elem) > acc._2) (elem, ages(elem)) else acc)

    favoriteColors.get(maxAgePerson._1)
  }


  def union[A](s1: Set[A], s2: Set[A]): Set[A] = {
    s1.foldLeft(s2)((acc, elem) => acc + elem)
  }

  def union[A](m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] = {
    m1.foldLeft(m2)((acc, elem) => acc.get(elem._1) match {

      case Some(value) =>
        val newEntry = elem._1 -> (elem._2 + value)
        acc + newEntry

      case None => acc + elem
    })
  }

  def union[A, B](m1: Map[A, B], m2: Map[A, B], comb: (B, B) => B): Map[A, B] = {
    m1.foldLeft(m2)((acc, elem) => acc.get(elem._1) match {

      case Some(value) =>
        val newEntry = comb(elem._2, value)
        acc + (elem._1 -> newEntry)

      case None => acc + elem
    })
  }

}
