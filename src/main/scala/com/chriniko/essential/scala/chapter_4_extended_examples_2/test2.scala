package com.chriniko.essential.scala.chapter_4_extended_examples_2

import scala.annotation.tailrec

object test2 {

  def main(args: Array[String]): Unit = {

    // --- my list test ---
    val clist = CList(1, 2, 3, 4, 5)
    println(clist)
    println(clist._head)
    println(clist._tail._head)
    println(clist._tail._tail._tail._tail._head)

    assert(clist == Entry(1, Entry(2, Entry(3, Entry(4, Entry(5, EmptyList()))))))

    println(clist.map(_ * 10))
    println(clist.mkString("[", ",", "]"))

    println
    println(EmptyList()._head)
    println(EmptyList()._tail)

    println

    val clistJsonArray = CList(JsonString("a string"), JsonNumber(1.0), JsonTrue)
    println(clistJsonArray)
    println()


    val j1 = JsonArray(CList(JsonString("a string"), JsonNumber(1.0), JsonTrue))
    println(j1.serialize)
    assert(j1.serialize == "[\"a string\", 1.0, true]")


    println

    // ---

    val j2 = JsonObject(
      CList(
        Member("firstname", JsonString("john")),
        Member("lastname", JsonString("doe")),
        Member("age", JsonNumber(27)),
        Member("previousExperience", JsonNull),
        Member("isProgrammer", JsonTrue),
        Member("rusty", JsonFalse),
        Member("languages", JsonArray(CList(JsonString("java"), JsonString("scala")))),
        Member("project", JsonObject(
          CList(
            Member("name", JsonString("learning-scala"))
          )
        ))
      )
    )
    println(j2.serialize)
    assert(
      j2.serialize ==
        "{\"firstname\" : \"john\", \"lastname\" : \"doe\", \"age\" : 27.0, \"previousExperience\" : null," +
          " \"isProgrammer\" : true, \"rusty\" : false, \"languages\" : [\"java\", \"scala\"]," +
          " \"project\" : {\"name\" : \"learning-scala\"}}"
    )


    // ---
    val j3 = JsonObject(
      CList(
        Member("a", JsonArray(CList(JsonNumber(1), JsonNumber(2), JsonNumber(3)))),
        Member("b", JsonArray(CList(JsonString("a"), JsonString("b"), JsonString("c")))),
        Member("c", JsonObject(CList(Member("doh", JsonTrue), Member("ray", JsonFalse), Member("me", JsonNumber(1))))
        ),
      )
    )

    println(j3.serialize)
    assert(
      j3.serialize == "{\"a\" : [1.0, 2.0, 3.0], " +
        "\"b\" : [\"a\", \"b\", \"c\"]," +
        " \"c\" : {\"doh\" : true, \"ray\" : false, \"me\" : 1.0}}"
    )

  }

}


// ---
sealed trait JsonElement

sealed trait JsonValue extends JsonElement {

  def serialize: String = {

    this match {
      case JsonObject(members) =>
        members.map(m => quote(m.label) + " : " + m.value.serialize).mkString("{", ", ", "}")

      case JsonArray(elements) => elements.map(_.serialize).mkString("[", ", ", "]")

      case JsonString(v) => quote(v)

      case JsonNumber(v) => v.toString

      case JsonTrue => true.toString

      case JsonFalse => false.toString

      case JsonNull => null
    }
  }

  private def quote(s: String): String = "\"" + s + "\""
}

final case class JsonObject(members: CList[Member]) extends JsonValue

final case class Member(label: String, value: JsonValue)

final case class JsonArray(elements: CList[JsonValue]) extends JsonValue

final case class JsonString(v: String) extends JsonValue

final case class JsonNumber(v: Double) extends JsonValue

case object JsonTrue extends JsonValue

case object JsonFalse extends JsonValue

case object JsonNull extends JsonValue

// ---

sealed trait CList[T] {
  def _head: Option[T]

  def _tail: CList[T]

  def map[Z](f: T => Z)(implicit c: CList[T] = this): CList[Z] = {
    c match {
      case Entry(h, t) => Entry(f(h), map(f)(t))
      case EmptyList() => EmptyList()
    }
  }

  def mkString(start: String = "", sep: String, end: String = ""): String = {

    @tailrec
    def helper(c: CList[T], acc: String): String = {
      c match {

        case Entry(h, t) =>
          val v = if (c._tail._head.isEmpty) acc + h else acc + h + sep
          helper(t, v)

        case EmptyList() => acc
      }
    }

    start + helper(this, "") + end
  }
}

object CList {

  def apply[T](elems: T*): CList[T] = {

    def helper(v: T*): CList[T] = {
      v match {
        case Nil => EmptyList()
        case h +: t => Entry(h, helper(t: _*))
      }
    }

    helper(elems: _*)
  }

}

case class Entry[T](head: T, tail: CList[T]) extends CList[T] {
  override def _head: Option[T] = Option(head)

  override def _tail: CList[T] = tail
}

case class EmptyList[T]() extends CList[T] {
  override def _head: Option[T] = None

  override def _tail: CList[T] = this
}
