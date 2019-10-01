package com.chriniko.essential.scala.chapter5

object test {

  def main(args: Array[String]): Unit = {

    println(Box("string"))
    println(Box("string").value)

    println(Box(1))
    println(Box(1).value)

    println(generic("string"))


    // ---

    val p = Pair(1, Pair(2, Pair(3, End())))
    println(p)
    println(p.length)

    // ---

    val example = Pair(1, Pair(2, Pair(3, End())))
    assert(example.contains(3))
    assert(example.contains(4) == false)
    assert(End().contains(0) == false)


    // ---

    val example2 = Pair(1, Pair(2, Pair(3, End())))
    assert(example2(0) == Success(1))
    assert(example2(1) == Success(2))
    assert(example2(2) == Success(3))
    assert(example2(3) == Failure("Not found"))


    // ---
    val sum = (x: Int, y: Int) => x + y: Int
    val sum3 = sum(3, _)
    println(sum3(10))


    // ---

    println
    println
    println("here")

    val example3 = IntPair(1, IntPair(2, IntPair(3, IntEnd)))
    println(example3.double)
    println(example3.product)
    println(example3.sum)
    println(example3.length)

    println

    println(example3.sum2)
    println(example3.product2)
    println(example3.length2)

    println

    println(example3.double3)
    println(example3.sum3)
    println(example3.product3)
    println(example3.length3)
  }

  def generic[A](in: A): A = in
}

// ---
final case class Box[A](value: A)

// ---

sealed trait LinkedList[T] {

  def contains(t: T): Boolean = {
    this match {
      case Pair(head, tail) => if (head == t) true else tail.contains(t)
      case End() => false
    }
  }

  def apply(nthElem: Int): Result[T] = {

    def helper(l: LinkedList[T], nthElem: Int, idx: Int): Result[T] = {
      l match {
        case Pair(head, tail) => if (idx == nthElem) Success(head) else helper(tail, nthElem, idx + 1)
        case End() => Failure("Not found")
      }
    }

    helper(this, nthElem, 0)
  }

  def length: Int = {
    this match {
      case Pair(_, tail) => 1 + tail.length
      case End() => 0
    }
  }

}

final case class Pair[T](head: T, tail: LinkedList[T]) extends LinkedList[T]

final case class End[T]() extends LinkedList[T]


sealed trait Result[A]

case class Success[A](result: A) extends Result[A]

case class Failure[A](reason: String) extends Result[A]

// ---

sealed trait IntLinkedList extends LinkedList[Int] {


  def product: Int = {
    this match {
      case IntPair(head, tail) => head * tail.product
      case IntEnd => 1
    }
  }

  def sum: Int = {
    this match {
      case IntPair(head, tail) => head + tail.sum
      case IntEnd => 0
    }
  }

  override def length: Int = {
    this match {
      case IntPair(_, tail) => 1 + tail.length
      case IntEnd => 0
    }
  }

  def double: IntLinkedList = {
    this match {
      case IntPair(head, tail) => IntPair(head * 2, tail.double)
      case IntEnd => IntEnd
    }
  }

  // ---

  def sum2: Int = fold(0, (h, t) => h + t)

  def product2: Int = fold(1, _ * _)

  def length2: Int = fold(0, (_, tail) => 1 + tail)

  private def fold(endValue: Int, f: (Int, Int) => Int): Int = {
    this match {
      case IntPair(head, tail) => f(head, tail.fold(endValue, f))
      case IntEnd => endValue
    }
  }

  // ---

  def sum3: Int = fold2[Int](0, _ + _)

  def product3: Int = fold2[Int](1, _ * _)

  def length3: Int = fold2[Int](0, (_, t) => 1 + t)

  private def fold2[E](endValue: E, f: (Int, E) => E): E = {
    this match {
      case IntPair(head, tail) => f(head, tail.fold2(endValue, f))
      case IntEnd => endValue
    }
  }

  def double3: IntLinkedList = fold2[IntLinkedList](IntEnd, (h, t) => IntPair(h * 2, t))

}

final case class IntPair(head: Int, tail: IntLinkedList) extends IntLinkedList

case object IntEnd extends IntLinkedList