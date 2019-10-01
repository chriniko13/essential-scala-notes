package com.chriniko.essential.scala.chapter5_fold

object test {

  def main(args: Array[String]): Unit = {

    // ---

    val l = Pair(1, Pair(2, Pair(3, End())))
    println(l)


    println(l.fold[Int](0, _ + _))

    println


    val l2 = Pair(Grade(2.0), Pair(Grade(3.5), Pair(Grade(5.0), End())))
    println(l2)


    val maxGrade = l2.fold[Grade](Grade(-1), (g1, g2) => if (g1.value > g2.value) g1 else g2)
    println(maxGrade)

    val maxGradeAsDouble = l2.fold[Double](-1, (g1, g2) => if (g1.value > g2) g1.value else g2)
    println(maxGradeAsDouble)

    // ---

    println

    val toFun = Sum.sum _
    println(toFun)
    println(toFun(1, 2))

    // ---

    println
    println

    val l3 = Pair(1, Pair(2, Pair(3, End())))
    println(l3)


    val add = (x: Int, y: Int) => x + y
    println(l3.fold2[Int](0)(add))

    println

    val maxGrade2 = l2.fold2[Grade](Grade(-1))((g1, g2) => if (g1.value > g2.value) g1 else g2)
    println(maxGrade2)

    println

    val maxGradeAsDouble2 = l2.fold2[Double](-1) { (g1, g2) => if (g1.value > g2) g1.value else g2 }
    println(maxGradeAsDouble2)

    // ---

    println("\n")

    val tree: Tree[String] =
      Node(
        Node(Leaf("To"), Leaf("iterate")),
        Node(
          Node(Leaf("is"), Leaf("human,")),
          Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))
        )
      )

    val acc = (acc:String, elem:String) => acc + " " + elem
    val result = tree.fold[String](acc)(x => x)
    println(result)

    val result2 = tree.fold2[String]((acc, elem) => acc + " " + elem)(x => x)
    println(result2)
  }

}


// ---

sealed trait LinkedList[A] {

  def fold[B](end: B, f: (A, B) => B): B = {
    this match {
      case Pair(head, tail) => f(head, tail.fold(end, f))
      case End() => end
    }
  }

  def fold2[B](end: B)(f: (A, B) => B): B = {
    this match {
      case Pair(head, tail) => f(head, tail.fold2(end)(f))
      case End() => end
    }
  }

}

final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

final case class End[A]() extends LinkedList[A]

// ---

case class Grade(value: Double)


// ---

object Sum {
  def sum(x: Int, y: Int): Int = x + y
}

// ---

sealed trait Tree[A] {

  def fold[B](acc: (B, B) => B)(f: A => B): B = {
    this match {
      case Node(l, r) => acc(l.fold(acc)(f), r.fold(acc)(f))
      case Leaf(v) => f(v)
    }
  }

  def fold2[B](acc: (B, B) => B)(f: A => B): B

}

final case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {

  override def fold2[B](acc: (B, B) => B)(f: A => B): B = {
      acc(l.fold2(acc)(f), r.fold2(acc)(f))
  }

}

final case class Leaf[A](v: A) extends Tree[A] {

  override def fold2[B](acc: (B, B) => B)(f: A => B): B = f(v)

}
