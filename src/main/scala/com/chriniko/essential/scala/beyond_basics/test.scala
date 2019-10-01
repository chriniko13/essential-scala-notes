package com.chriniko.essential.scala.beyond_basics

import scala.concurrent.Future
import scala.language.{higherKinds, implicitConversions}

object test {

  def main(args: Array[String]): Unit = {

    // ---
    import com.chriniko.essential.scala.beyond_basics.AnimalConverter._
    val c = Cat("meow")
    val d: Dog = c

    println(d)

    // ---

  }

}


//___________________DOMAIN

case class Person(name: String)

case class Customer(customerNumber: Int, p: Person)


//___________________INFRA

trait Serializer {

  def serialize(c: Customer): HttpResponse

  def deserialize(input: HttpRequest): Person

}

class DatabaseConnection {

}

case class HttpRequest()

case class HttpResponse()


// ___________________FP APPROACH

class WebServer {

  val databaseConnection = new DatabaseConnection

  type WebRequest = HttpRequest => HttpResponse // Note: type alias.


  val deSerializePerson: HttpRequest => Person = ???

  val createCustomer: Person => Customer = ???

  // ---
  // first way...
  val saveCustomer: Customer => Customer = DataAccess.saveCustomer(databaseConnection)

  // another way...
  val dac = new DataAccessClass(databaseConnection)
  val saveCustomer2: Customer => Customer = dac.saveCustomer

  // just another way...
  implicit val dbConn: DatabaseConnection = new DatabaseConnection
  val saveCustomer3: Customer => Customer = DataAccessImplicit.saveCustomer
  // ---

  val serializeCustomer: Customer => HttpResponse = ???


  val registerCustomer: WebRequest =
    deSerializePerson andThen
    createCustomer andThen
    saveCustomer andThen
    serializeCustomer




  object DataAccess {
    def saveCustomer(databaseConnection: DatabaseConnection)(c: Customer): Customer = ???
  }

  class DataAccessClass(databaseConnection: DatabaseConnection) {
    def saveCustomer(c: Customer): Customer = ???
  }

  object DataAccessImplicit {
    def saveCustomer(c: Customer)(implicit databaseConnection: DatabaseConnection): Customer = ???
  }


  // ___________________HIGHER KINDED TYPES INTRO

  // abstracting over higher kinded types...
  type WebRequest2 = HttpRequest => Future[HttpResponse]

  type WebRequest3 = HttpRequest => Either[Error, HttpResponse]

  val createCustomerF: Person => Future[Customer] = ???
  val saveCustomerF: Customer => Future[Customer] = ???


  val fstStep: HttpRequest => Future[Customer] = deSerializePerson andThen createCustomerF
  //saveCustomerF
  //serializeCustomer

  import scala.concurrent.ExecutionContext.Implicits._

  val customerSaved: HttpRequest => Future[HttpResponse] = hr => fstStep(hr).flatMap(saveCustomerF).map(serializeCustomer)



  // ___________________HIGHER KINDED TYPES IMPL. WITH EITHER

  trait Mappable[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  abstract class Abstracted[F[_] : Mappable] {
    def _deserializeCustomer: HttpRequest => Person = ???
    def _createCustomer: Person => F[Customer] = ???
    def _saveCustomer: Customer => F[Customer] = ???
    def _serializeCustomer: Customer => HttpResponse = ???


    val first: HttpRequest => F[Customer] = _deserializeCustomer andThen _createCustomer

    //val full: HttpRequest => F[HttpResponse] = hr => first(hr).flatMap(_saveCustomer).map(_serializeCustomer)


    // ___________________PRINCIPLED TYPED CONVERSIONS
    implicit val futureMappable: Mappable[Future] = new Mappable[Future] {
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

      override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
    }

    val withAFuture: Abstracted[Future] = new Abstracted[Future]() {}
  }

}

// ___________________IMPLICITS
case class Cat(name: String)
case class Dog(name: String, status: String)

object AnimalConverter {
  implicit def transform(c: Cat): Dog = Dog(c.name, "implicit_type_conversion")
}


// ___________________LAYERED APPROACH


class WebController(service: ApplicationService, serializer: Serializer) {

  def register(request: HttpRequest): HttpResponse = {
    val person = serializer.deserialize(request)
    val storedEntity = service.register(person)
    serializer.serialize(storedEntity)
  }

}


class ApplicationService(datastore: CustomerRepo) {

  def register(p: Person): Customer = {
    //validate them?
    // generate q unique customer number?
    val c = Customer(1, p)
    datastore.saveCustomer(c)
    c
  }

}

class CustomerRepo {
  def saveCustomer(c: Customer): Unit = {

  }

}





