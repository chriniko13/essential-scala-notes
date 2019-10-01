package com.chriniko.essential.scala.chapter6_for_comprehension

object test {


  def main(args: Array[String]): Unit = {

    // ---
    println(Seq(1, 2, 3).map(_ * 2))

    val s = for {
      x <- Seq(1, 2, 3)
    } yield x * 2

    println(s)


    // ---
    println

    val data = Seq(Seq(1), Seq(2, 3), Seq(4, 5, 6))
    println(data.flatMap(_.map(_ * 2)))


    val data2 = for {
      subseq <- data
      element <- subseq
    } yield element * 2
    println(data2)


    // ---
    println

    val r: Unit = for (
      elem <- Seq(1, 2, 3)
    ) println(elem * 2)

    println(r)


    // ---
    println
    println(readInt("123"))
    println(readInt("abc"))

    println(readInt("abc").getOrElse(1))

    readInt("123") match {
      case Some(v) => println(v + 1)
      case None => println("nope!")
    }

    println(sum(Some(1), Some(2)))
    println(sum(None, Some(2)))
    println(sum(Some(2), None))

    sum(Option(10), Option(20)) match {
      case Some(value) => println(value)
      case None => println("nothing")
    }

    println(Seq(Some(1), None, Some(3)).flatMap(x => x))
    println(Seq(Some(1), None, Some(3)).flatten)


    // ---
    println

    val o1: Option[Int] = Some(1)
    val o2: Option[Int] = Some(1)

    val o3 = for {
      v1 <- o1
      v2 <- o2
    } yield v1 + v2

    println(o3)

    // ---
    println

    println
    println(addOptions(Some(1), Some(1)))
    println(addOptions(Some(1), None))
    println(addOptions(None, Some(1)))

    println
    println(addOptions2(Some(1), Some(1)))
    println(addOptions2(Some(1), None))
    println(addOptions2(None, Some(1)))

    println
    println(addOptions(Some(1), Some(1), Some(1)))
    println(addOptions(Some(1), None, Some(1)))
    println(addOptions(None, Some(1), None))

    println
    println(addOptions2(Some(1), Some(1), Some(1)))
    println(addOptions2(Some(1), None, Some(1)))
    println(addOptions2(None, Some(1), None))


    // ---
    println

    println
    println(divide(10, 2))
    println(divide(10, 0))

    println
    println(divideOptions(Some(10), Some(2)))
    println(divideOptions(Some(10), Some(0)))
    println(divideOptions(Some(10), None))


    // ---
    println

    println
    calculator("1", "+", "2")
    calculator("1", "-", "2")
    calculator("1", "*", "2")
    calculator("10", "/", "2")
    calculator("10", "/", "0")
    calculator("a", "+", "2")
    calculator("1", "+", "b")
    calculator("1", "n", "2")
    calculator("a", "n", "b")

    println
    calculator2("1", "+", "2")
    calculator2("1", "-", "2")
    calculator2("1", "*", "2")
    calculator2("10", "/", "2")
    calculator2("10", "/", "0")
    calculator2("a", "+", "2")
    calculator2("1", "+", "b")
    calculator2("1", "n", "2")
    calculator2("a", "n", "b")
  }


  // ---
  def readInt(str: String): Option[Int] =
    if (str matches "-?\\d+") Some(str.toInt) else None

  // Note: compose computations.
  def sum(a: Option[Int], b: Option[Int]): Option[Int] = {
    a.flatMap(aVal => b.map(bVal => aVal + bVal))
  }


  // ---

  def addOptions(a: Option[Int], b: Option[Int]): Option[Int] = {
    for {
      v1 <- a
      v2 <- b
    } yield v1 + v2
  }

  def addOptions2(a: Option[Int], b: Option[Int]): Option[Int] = {
    a.flatMap(aVal => b.map(bVal => aVal + bVal))
  }

  def addOptions(a: Option[Int], b: Option[Int], c: Option[Int]): Option[Int] = {
    for {
      v1 <- a
      v2 <- b
      v3 <- c
    } yield v1 + v2 + v3
  }

  def addOptions2(a: Option[Int], b: Option[Int], c: Option[Int]): Option[Int] = {
    a.flatMap(aVal => b.flatMap(bVal => c.map(cVal => aVal + bVal + cVal)))
  }


  def divide(a: Int, b: Int): Option[Int] = {
    b match {
      case 0 => None
      case _ => Some(a / b)
    }
  }

  def divideOptions(a: Option[Int], b: Option[Int]): Option[Int] = {
    for {
      v1 <- a
      v2 <- b
      r <- divide(v1, v2)
    } yield r
  }

  def calculator(operand1: String, operator: String, operand2: String): Unit = {

    val r: Option[Int] = for {
      v1 <- readInt(operand1)
      v2 <- readInt(operand2)
      result <- operator match {
        case "+" => Some(v1 + v2)
        case "-" => Some(v1 - v2)
        case "*" => Some(v1 * v2)
        case "/" => if (v2 == 0) None else Some(v1 / v2)
        case _ => None
      }
    } yield result

    r match {
      case Some(v) => println(s"result is: $v")
      case None => println(s"no correct provided parameters, $operand1 $operator $operand2")
    }

  }

  def calculator2(operand1: String, operator: String, operand2: String): Unit = {

    def performOperator(o1: Int, o2: Int, op: String): Option[Int] = {
      op match {
        case "+" => Some(o1 + o2)
        case "-" => Some(o1 - o2)
        case "*" => Some(o1 * o2)
        case "/" => if (o2 == 0) None else Some(o1 / o2)
        case _ => None
      }
    }

    val r = readInt(operand1).flatMap(o1 => readInt(operand2).flatMap(o2 => performOperator(o1, o2, operator)))
    r match {
      case Some(v) => println(s"result is: $v")
      case None => println(s"no correct provided parameters, $operand1 $operator $operand2")
    }

  }

  }

// ---
// Important Note: 6.3.1 Exercises are in test class of chapter6 package.
