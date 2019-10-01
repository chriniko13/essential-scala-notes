package com.chriniko.essential.scala.chapter4_recursive_data

import scala.annotation.tailrec

object test {

  def main(args: Array[String]): Unit = {


    val l = Pair(1, Pair(2, Pair(3, End)))
    println(l)
    println(l.tail)
    println(s"sum is ${l.sum()}")

    assert(l.sum() == 6)
    assert(l.tail.sum() == 5)
    assert(End.sum() == 0)

    println

    println(IntList.sum2(l))


    // ----
    println

    val example = Pair(1, Pair(2, Pair(3, End)))
    assert(example.length == 3)
    assert(example.tail.length == 2)
    assert(End.length == 0)

    println(example.length())

    println

    println(example.product())
    assert(example.product == 6)
    assert(example.tail.product == 6)
    assert(End.product == 1)

    println


    println(example.double())
    assert(example.double == Pair(2, Pair(4, Pair(6, End))))

    println(example.tail.double())
    assert(example.tail.double == Pair(4, Pair(6, End)))

    println(End.double())
    assert(End.double == End)


    // ---
    println
    println

    val t = Node(
      Node(
        Node(Leaf(1), Leaf(2)),
        Leaf(3)
      ),

      Node(
        Node(Leaf(10), Node(Node(Leaf(11), Leaf(1)), Leaf(0))),
        Leaf(12)
      )
    )

    println(t.sum)
    assert(t.sum == 40)

    println

    println(t._sum)
    assert(t._sum == 40)

    println

    println(t.double)
    assert(
      t.double ==

        Node(
          Node(
            Node(Leaf(2), Leaf(4)), Leaf(6)
          ),

        Node(
          Node(
            Leaf(20),
            Node(
              Node(Leaf(22), Leaf(2)), Leaf(0)
            )
          ),
          Leaf(24))
        )
    )

    println
    println(t._double)
    assert(
      t._double ==

        Node(
          Node(
            Node(Leaf(2), Leaf(4)), Leaf(6)
          ),

          Node(
            Node(
              Leaf(20),
              Node(
                Node(Leaf(22), Leaf(2)), Leaf(0)
              )
            ),
            Leaf(24))
        )
    )
  }


}

// ---

// IntList = Pair(head * tail) + End()
sealed trait IntList {

  def sum(): Int = {
    @tailrec
    def helper(p: IntList, acc: Int): Int = {
      p match {
        case Pair(head, tail) => helper(tail, acc + head)
        case End => acc
      }
    }

    helper(this, 0)
  }

  def length(): Int = {
    @tailrec
    def helper(s: IntList, acc: Int): Int = {
      s match {
        case End => acc
        case Pair(head, tail) => helper(tail, acc + 1)
      }
    }

    helper(this, 0)
  }

  def product(): Int = {
    @tailrec
    def helper(s: IntList, acc: Int): Int = {
      s match {
        case End => acc
        case Pair(head, tail) => helper(tail, acc * head)
      }
    }

    helper(this, 1)
  }

  def double(): IntList = {
    this match {
      case End => End
      case Pair(head, tail) => Pair(2 * head, tail.double())
    }
  }

}

case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList


object IntList {

  // non tail recursive
  def sum2(l: IntList): Int = {
    l match {
      case End => 0
      case Pair(head, tail) => head + sum2(tail)
    }
  }
}


// ---

// Tree = Node(left*right) + Lead(v)

sealed trait Tree {

  def _sum: Int
  def _double: Tree

  def sum: Int = {
    this match {
      case Node(left, right) =>
        left.sum + right.sum
      case Leaf(v) => v
    }
  }

  def double: Tree = {
    this match {
      case Leaf(v) => Leaf(v * 2)
      case Node(left, right) => Node(left.double, right.double)
    }
  }
}

final case class Node(left: Tree, right: Tree) extends Tree {
  override def _sum: Int = left.sum + right.sum

  override def _double: Tree = Node(left.double, right.double)
}

final case class Leaf(v: Int) extends Tree {
  override def _sum: Int = v

  override def _double: Tree = Leaf(2 * v)
}