package com.chriniko.essential.scala.chapter6

import scala.annotation.tailrec

object test {


  def main(args: Array[String]): Unit = {

    // ---
    println

    val l = Seq(1, 2, 3)
    println(l)

    println(l(0))
    //println(l(15))
    println(l.headOption)
    println(l.contains(2))
    println(l.find(_ == 3))
    println(l.filter(_ > 1))
    println(l.sortWith(_ > _))

    println(l :+ 4)

    println(0 +: l)

    println(Seq(1, 2, 3) ++ Seq(4, 5, 6))

    // ---
    println

    println(Nil)

    println(1 :: 2 :: 3 :: Nil)
    println(List(1, 2, 3) ::: List(4, 5, 6))

    // ---
    println

    val animals = Seq("cat", "dog", "penguin")
    println(animals)

    val animals2 = "mouse" +: animals :+ "tyrannosaurus"
    println(animals2)

    println(2 +: animals2)


    // ---
    println

    println(MovieDB.bornBefore(1940).map(d => d.firstName + " " + d.lastName))

    println(MovieDB.moreThanNumberOfFilms(4).map(d => d.firstName + " " + d.lastName))

    println(MovieDB.bornBeforeAndMoreThanNumberOfFilms(1990, 1).map(d => d.firstName + " " + d.lastName))

    println(MovieDB.sort(true).map(d => d.firstName + " " + d.lastName))

    println(MovieDB.sort(false).map(d => d.firstName + " " + d.lastName))


    // ---
    println()

    println(Seq(1, 2, 3).map(_ * 2))
    println("dog".toSeq.permutations.toList)

    println(Seq("cat", "dog").flatMap(_.toSeq.permutations.toList))

    println(Seq("cat", "dog").foldLeft("")(_ + _))
    println(Seq("cat", "dog").foldRight("")(_ + _))

    Seq("cat", "dog").foreach(println(_))


    // ---
    println
    println(MovieDB.nolanFilms)
    println(MovieDB.cinephile)
    println(MovieDB.vintageMcTiernan)
    println(MovieDB.vintageMcTiernan2)
    println(MovieDB.highScoreTable)
    println(MovieDB.averageScoreAcrossAllFilms)
    MovieDB.tonightsListing
    println
    MovieDB.fromTheArchives.foreach(println)


    // ---
    println

    println
    println(min(Seq(1, 2, 3, 4, -5)))
    println(min2(Seq(1, 2, 3, 4, -5)))

    println
    println(unique(Seq(1, 1, 2, 2, 3, 4, -5, 5, 5)))
    println(unique2(Seq(1, 1, 2, 2, 3, 4, -5, 5, 5)))

    println
    println(reverse(Seq(5, 4, 3, 2, 1)))
    println(reverse2(Seq(5, 4, 3, 2, 1)))

    println
    println(map(Seq(1, 2, 3), _ * 2))
    println(map2(Seq(1, 2, 3), _ * 2))

    println
    println(foldLeft[Int](Seq(1, 2, 3), 0, (acc, elem) => acc + elem))
    println(foldLeft2[String, String](Seq("a", "b", "c"), "", (acc, elem) => acc + elem))


    // ---
    println

    println(MovieDB.nolanFilms2)
    println(MovieDB.cinephile2)
    println(MovieDB.highScoreTable2)
    MovieDB.tonightsListing2

  }


  def min(s: Seq[Int]): Int = {
    @tailrec
    def helper(s: Seq[Int], min: Int): Int = {
      s match {
        case head :: tail => if (head < min) helper(tail, head) else helper(tail, min)
        case _ => min
      }
    }

    helper(s, s.head)
  }

  def min2(s: Seq[Int]): Int = {
    s.foldLeft(Int.MaxValue)((x, y) => if (x < y) x else y)
  }

  def unique(s: Seq[Int]): Seq[Int] = {
    @tailrec
    def helper(s: Seq[Int], acc: Seq[Int]): Seq[Int] = {
      s match {
        case head :: tail => if (acc.contains(head)) helper(tail, acc) else helper(tail, acc :+ head)
        case _ => acc
      }
    }
    helper(s, Seq())
  }


  def unique2(s: Seq[Int]): Seq[Int] = {
    s.foldLeft(Seq[Int]())((acc, elem) => if (acc.contains(elem)) acc else acc :+ elem)
  }

  def reverse(s: Seq[Int]): Seq[Int] = {
    @tailrec
    def helper(s: Seq[Int], acc: Seq[Int], pos: Int): Seq[Int] = {
      pos match {
        case -1 => acc
        case _ => helper(s, acc :+ s(pos), pos - 1)
      }
    }
    helper(s, Seq(), s.length - 1)
  }

  def reverse2(s: Seq[Int]): Seq[Int] = {
    s.foldRight(Seq.empty[Int])((elem, acc) => acc :+ elem)
  }

  def map[A](s: Seq[Int], f: Int => A): Seq[A] = {
    def helper(s: Seq[Int], f: Int => A, acc: Seq[A]): Seq[A] = {
      s match {
        case head :: tail => helper(tail, f, acc :+ f(head))
        case _ => acc
      }
    }
    helper(s, f, Seq())
  }

  def map2[A](s: Seq[Int], f: Int => A): Seq[A] = {
    s.foldRight(Seq[A]())((elem, acc) => f(elem) +: acc)
  }


  def foldLeft[B](s: Seq[Int], init: B, f: (B, Int) => B): B = {

    var result = init

    s.foreach(elem => {
      result = f(result, elem)
    })

    result
  }

  def foldLeft2[A, B](s: Seq[A], init: B, f: (B, A) => B): B = {

    var result = init

    s.foreach(elem => {
      result = f(result, elem)
    })

    result
  }


}

// ---

object MovieDB {

  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])

  val memento = Film("Memento", 2000, 8.5)

  val darkKnight = Film("Dark Knight", 2008, 9.0)

  val inception = Film("Inception", 2010, 8.8)

  val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7)

  val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9)

  val unforgiven = Film("Unforgiven", 1992, 8.3)

  val granTorino = Film("Gran Torino", 2008, 8.2)

  val invictus = Film("Invictus", 2009, 7.4)

  val predator = Film("Predator", 1987, 7.9)

  val dieHard = Film("Die Hard", 1988, 8.3)

  val huntForRedOctober = Film("The Hunt for Red October", 1990, 7.6)

  val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus)
  )

  val mcTiernan = Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair)
  )

  val nolan = Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception)
  )

  val someGuy = Director("Just", "Some Guy", 1990,
    Seq()
  )


  val directors: Seq[Director] = Seq(eastwood, mcTiernan, nolan, someGuy)


  // ---

  def moreThanNumberOfFilms(numberOfFilms: Int): Seq[Director] = directors.filter(_.films.length > numberOfFilms)

  def bornBefore(year: Int): Option[Director] = directors.find(_.yearOfBirth < year)

  def bornBeforeAndMoreThanNumberOfFilms(year: Int, numberOfFilms: Int): Seq[Director] = {
    directors.filter(d => d.films.length > numberOfFilms && d.yearOfBirth < year)
  }

  def sort(ascending: Boolean): Seq[Director] = {
    val func =
      if (ascending) (d1: Director, d2: Director) => d1.yearOfBirth < d2.yearOfBirth
      else (d1: Director, d2: Director) => d1.yearOfBirth > d2.yearOfBirth

    directors.sortWith(func)
  }


  // ---
  val nolanFilms: Seq[String] = nolan.films.map(_.name)

  val cinephile: Seq[String] = directors.flatMap(_.films).map(_.name)

  val vintageMcTiernan: Int = mcTiernan.films.reduce((f1, f2) => if (f1.yearOfRelease < f2.yearOfRelease) f1 else f2).yearOfRelease

  val vintageMcTiernan2: Int = mcTiernan.films.minBy(_.yearOfRelease).yearOfRelease

  val vintageMcTiernan3: Int = mcTiernan.films.map(_.yearOfRelease).min

  val highScoreTable: Seq[Film] = directors.flatMap(_.films).sortBy(_.imdbRating).reverse

  val averageScoreAcrossAllFilms: Double
  = directors.flatMap(_.films).map(_.imdbRating).foldLeft(0.0D)(_ + _) / directors.flatMap(_.films).size


  lazy val tonightsListing: Unit
  = directors
    .map(d => (d.films, d))
    .foreach { t =>

      val films = t._1
      val directorName = t._2.firstName + " " + t._2.lastName

      films.foreach { f =>
        println(s"Tonight only! ${f.name} by $directorName")
      }
    }

  val fromTheArchives: Seq[(String, Option[Film])]
  = directors.map(d => (d.firstName + " " + d.lastName, d.films.sortBy(_.yearOfRelease).headOption))


  // --

  val nolanFilms2: Seq[String] = for {
    film <- nolan.films
  } yield film.name

  val cinephile2: Seq[String] = for {
    director <- directors
    film <- director.films
  } yield film.name

  val highScoreTable2: Seq[Film] = (for {
    director <- directors
    film <-director.films
  } yield film).sortBy(_.imdbRating).reverse

  lazy val tonightsListing2: Unit = for {
    director <- directors
    film <- director.films
  } println(s"Tonight only! ${film.name} by ${director.firstName} ${director.lastName}")
}
