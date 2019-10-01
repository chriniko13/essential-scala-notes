package com.chriniko.essential.scala.real_world_app_purely_func_techniques

import scala.util.Try

object test {

  def main(args: Array[String]): Unit = {

    // ---
    println
    case class Foo(x: Int, y: Int)

    val o1 = Option(100)
    val o2 = Option(2000) // None

    val f = for {
      x<-o1
      y<-o2
    } yield Foo(x, y)

    println(f)



    // ---
    println
    val emitRecords = Source.emit(Config.current)_
    val results = go(emitRecords)
    println(s"results: $results")


  }

  private def go(emit: () => Seq[RawUser]): Result = {
    println("started...")
    emit().map(rawUser => transform(rawUser)).foldLeft(Result(0, 0))((acc, elem) => acc + toResult(elem))
  }

  private def transform(r: RawUser): Either[TransformError, DomainUser] = {

    val maybePerson = r.person
    val maybePhoneNumber = PhoneNumber.from(r.telephone)

    // Note: strip the monads.
    for {
      person <- maybePerson
      phoneNumber <- maybePhoneNumber
    } yield DomainUser(person, phoneNumber)
  }

  private def toResult(transformedRecord: Either[TransformError, DomainUser]): Result = {
    transformedRecord match {
      case Right(_) => Result(1, 0)
      case Left(_) => Result(0, 1)
    }
  }

}


// ---------------------------------------------------------------------------------------------------------------------
// [FP TOOLBOX]
// Semigroup
// Applicative
// Monad
// Monoid
// Functor
// Disjunction


// Railway Pattern
// {start}     Source ---success---> Transform ---success---> Send            {end}
// {start}             ---fail--->             ---success--->                 {end}


// ---------------------------------------------------------------------------------------------------------------------

object Source {
  def emit(conf: Config)(): Seq[RawUser] = {
    // do something with conf
    RawData.generateRawUsers
  }
}


// ---------------------------------------------------------------------------------------------------------------------

object RawData {
  def generateRawUsers: Seq[RawUser] = {
    (1 to 1000).map {idx => RawUser(s"name$idx", s"email$idx", s"telephone-$idx", s"address$idx", s"city$idx", s"zipcode$idx")}
  }
}

case class RawUser(name: String, email: String, telephone: String, address: String, city: String, zipcode: String) {

  lazy val person: Either[TransformError, Person] = {
    name.split(" ").toList match {
      case first :: last :: Nil => Right(Person(first, last))
      case _ => Left(TransformError(s"not valid person name, name: $name"))
    }
  }
}
// ---------------------------------------------------------------------------------------------------------------------

trait Config
case class FileConfig() extends Config

object Config {
  def current: Config = FileConfig()
}

// ---------------------------------------------------------------------------------------------------------------------

case class Result(successes: Int, failures: Int) {

  def +(other: Result) : Result = {
    Result(this.successes + other.successes, this.failures + other.failures)
  }

}
case class TransformError(error:String)

case class DomainUser(person: Person, phoneNumber: PhoneNumber)
case class Person(firstname: String, lastname: String)
case class PhoneNumber(firstPart: String, secondPart: Int)

object PhoneNumber {

  private def toInt(s: String): Either[TransformError, Int] = {
    Try(s.toInt).toEither.left.map(error => TransformError(error.toString))
  }

  private def toTupleInfo(phoneString: String): Either[TransformError, (String, String)] = {
    Option(phoneString)
      .map(_.split("-"))
      .filter(_.length == 2)
      .map(a => (a(0), a(1)))
      .toRight(TransformError(s"not valid phone, phone: $phoneString"))
  }

  def from(phoneString: String): Either[TransformError, PhoneNumber] = {
    for {
      x1 <- toTupleInfo(phoneString)
      x2 <- toInt(x1._2)
    } yield PhoneNumber(x1._1, x2)
  }
}