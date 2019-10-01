package com.chriniko.essential.scala.monad

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object test {


  def main(args: Array[String]): Unit = {


    // ---
    val result = UserService.loadUser("user2")
      .flatMap(x => x.child)
      .flatMap(User.getChild)

    println(s"result: $result")

    result match {
      case Some(value) => println(s"value: $value")
      case None => println("no value")
    }


    // ---
    println

    val result2: Option[User] = for {
      u <- UserService.loadUser("user2")
      c1 <- u.child
      c2 <- c1.child
    } yield c2

    println(s"result2: $result2")


    // ---
    println

    println(Await.result(SampleApp.result("user1"), Duration.Inf))

  }

}


// ----

trait M[A] {

  def flatMap[B](f: A => M[B]): M[B]

  def unit(x: A): M[A]

  def map[B](f: A => B): M[B]

  def flatten(v: M[M[A]]): M[A]

}


// ---

sealed trait User {
  val child: Option[User]
}

final case class StdUser(child: Option[User]) extends User

object UserService {

  private val db = Map(
    "user1" -> StdUser(Some(StdUser(None))),
    "user2" -> StdUser(Some(StdUser(Some(StdUser(None))))),
    "user3" -> StdUser(Some(StdUser(Some(StdUser(Some(StdUser(None)))))))
  )

  def loadUser(name: String): Option[User] = db.get(name)
}

object User {
  val getChild: User => Option[User] = _.child
}

// ---

case class Order(id: Int)

case class Item(id: Int)

case class PurchaseResult(result: String)

case class LogResult(result: String)

import scala.concurrent.ExecutionContext.Implicits.global

object SampleApp {
  def loadOrder(username: String): Future[Order] = Future {
    Order(1)
  }

  def loadItem(order: Order): Future[Item] = Future {
    Item(1)
  }

  def purchaseItem(item: Item): Future[PurchaseResult] = Future {
    PurchaseResult("2")
  }

  def logPurchase(purchaseResult: PurchaseResult): Future[LogResult] = Future {
    LogResult("2 -- log")
  }


  val result: String => Future[LogResult] = username => loadOrder(username)
    .flatMap(loadItem)
    .flatMap(purchaseItem)
    .flatMap(logPurchase)
}

