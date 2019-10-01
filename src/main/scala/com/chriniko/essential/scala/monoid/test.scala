package com.chriniko.essential.scala.monoid

object test {

  def main(args: Array[String]): Unit = {


    // ---
    println

    import Monoid._

    val stringMonoid = new StringMonoid
    val result = mconcat(Seq("a","b","c","d"))(stringMonoid)
    println(result)


    println
    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def mzero: Int = 0

      override def mappend(a: Int, b: Int): Int = a + b
    }
    val result2 = mconcat(Seq(1,2,3))
    println(result2)


    // ---
    println


  }

}

// ---
trait Monoid[A] {

  def mzero: A

  def mappend(a: A, b: A) : A

}

object Monoid {

  def mconcat[A](s: Seq[A])(implicit env: Monoid[A]): A =
    s.foldLeft(env.mzero)(env.mappend)

}

class StringMonoid extends Monoid[String] {
  override def mzero: String = ""

  override def mappend(a: String, b: String): String = a + b
}


// Note: typeclasse covergence


// ---
sealed trait JsonElem

trait ToJson[A] {
  def toJson(v: A): JsonElem
}
