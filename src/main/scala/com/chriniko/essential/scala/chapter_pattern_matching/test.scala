package com.chriniko.essential.scala.chapter_pattern_matching

object test {

  def main(args: Array[String]): Unit = {

    // ---
    println()


    println("hi!") match {
      case () => println("it's a unit")
    }

    val X = 1
    val Y = 2
    val Z = 3
    val W = 4

    1 match {
      case X => println("X")
      case Y => println("Y")
      case Z | W => println("Z")
    }

    (1, "a") match {
      case (a, b) => println(s"$a, $b")
    }

    // ---
    println()

    Seq(1,2,3) match {
      case Seq(a, b, c) => println(s"$a $b $c")
      case Nil => println("empty")
    }

    Seq(1,2,3) match {
      case head :: tail => println(s"$head :: $tail")
    }


    // ---
    println()

    class Email(val value: String)

    object Email {

      def apply(arg:String): Email = new Email(arg)

      def unapply(arg: String): Option[(String, String)] = {
        println("unapply called")
        Option(arg).map(_.split("@")).filter(_.length == 2).map(arr => (arr(0), arr(1)))
      }
    }

    object Uppercase {
      def unapply(arg: String): Option[String] = Some(arg.toUpperCase)
    }


    "some@mail.com" match {
      case Email(firstPart, Uppercase(secondPart)) => println(s"$firstPart --- $secondPart")
      case _ => println("nothing...")
    }

    "some" match {
      case Email(firstPart, secondPart) => println(s"$firstPart --- $secondPart")
      case _ => println("nothing...")
    }


    // ---
    println()

    object Words {
      def unapplySeq(str: String): Option[Seq[String]] = {
        Option(str).map(_.split(" "))
      }
    }

    "the quick brown fox" match {
      case Words(w1, w2) => println(s"two words, ($w1 $w2)")
      case Words(w1, tail@_*) => println(s"two words, ($w1 ...$tail)")

      case Words(w1, w2, w3) => println(s"three words, ($w1 $w2 $w3)")
      case Words(w1, w2, w3, w4) => println(s"four words, ($w1 $w2 $w3 $w4)")

    }


    // ---
    println()

    class Positive(val i: Int)

    object Positive {

      def apply(i: Int): Positive = new Positive(i)

      def unapply(arg: Int): Option[String] = {
        arg match {
          case a: Int if a > 0 => Some("Yes")
          case _ => None
        }
      }
    }

    assert(
      "No" ==
        (0 match {
          case Positive(_) => "Yes"
          case _ => "No"
        })
    )

    assert(
      "Yes" ==
        (42 match {
          case Positive(_) => "Yes"
          case _ => "No"
        })
    )


    // ---
    println()

    object Titlecase {
      def unapply(arg: String): Option[String] = {
        Some(arg)
          .map(_.split(" "))
          .map(array => array.map(word => word.charAt(0).toUpper.toString + word.substring(1)).mkString(" "))
      }
    }

    assert(
      "Sir Lord Doctor David Gurnell" ==
        ("sir lord doctor david gurnell" match {
          case Titlecase(str) =>
            println(str)
            str
        })
    )
  }

}
