package com.chriniko.essential.scala.chapter7_json_exercise

import java.util.Date

object test {

  def main(args: Array[String]): Unit = {

    // ---
    println()

    val obj = JsObject(
      Map(
        "foo" -> JsString("a"),
        "bar" -> JsString("b"),
        "baz" -> JsString("c")
      )
    )

    println(obj.stringify)


    // ---
    println()

    {
      val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))

      import VisitorImplicits.visitorJsWriter


      val serializedVisitors = visitors.map(JsWriter[Visitor](_))
      serializedVisitors.foreach(println)

      println()

      val stringifiedVisitors = serializedVisitors.map(_.stringify)
      stringifiedVisitors.foreach(println)

    }


    // ---
    println()

    {

      import VisitorImplicits._
      import JsWriterImplicits._

      val jsValue = Anonymous("001", new Date).toJson
      println(jsValue)
      println(jsValue.stringify)

    }


  }


}


// ---

sealed trait JsValue {
  def stringify: String
}

final case class JsObject(values: Map[String, JsValue]) extends JsValue {
  override def stringify: String = {
    values.map[String](t => "\"" + t._1 + "\":" + t._2.stringify).mkString("{", ",", "}")
  }
}

final case class JsString(value: String) extends JsValue {
  override def stringify: String = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
}

// ---

//type class connects/converts a X ADT to a Y ADT
trait JsWriter[T] {
  def write(t: T): JsValue
}

// 1st option
object JsWriter {
  def apply[T](t: T)(implicit instance: JsWriter[T]): JsValue = {
    instance.write(t)
  }
}

// 2nd option
object JsWriterImplicits {

  implicit class JsWriterOps[T](t: T) {
    def toJson(implicit instance: JsWriter[T]): JsValue = {
      instance.write(t)
    }
  }

}


// ---

sealed trait Visitor {
  def id: String

  def createdAt: Date

  def age: Long = new Date().getTime - createdAt.getTime
}

final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

final case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor


object VisitorImplicits {

  implicit val anonymousJsWriter: JsWriter[Anonymous] = new JsWriter[Anonymous] {
    override def write(t: Anonymous): JsValue = {
      JsObject(Map(
        "id" -> JsString(t.id),
        "createdAt" -> JsString(t.createdAt.toString)
      ))
    }
  }

  implicit val userJsWriter: JsWriter[User] = new JsWriter[User] {
    override def write(t: User): JsValue = {
      JsObject(Map(
        "id" -> JsString(t.id),
        "email" -> JsString(t.email),
        "createdAt" -> JsString(t.createdAt.toString)
      ))
    }
  }

  implicit val visitorJsWriter: JsWriter[Visitor] = new JsWriter[Visitor] {

    override def write(t: Visitor): JsValue = {
      t match {
        case a@Anonymous(_, _) => JsWriter[Anonymous](a)
        case u@User(_, _, _) => JsWriter[User](u)
      }
    }
  }
}