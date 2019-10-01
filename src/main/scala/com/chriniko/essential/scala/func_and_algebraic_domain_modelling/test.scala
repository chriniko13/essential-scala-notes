package com.chriniko.essential.scala.func_and_algebraic_domain_modelling

import cats.Monad

import scala.language.higherKinds

object test {


  def main(args: Array[String]): Unit = {


  }

}

// ---

// functions aggregate upwards into modules
trait Trading[F[_] /*effect type*/] {

  def fromClientOrder: ClientOrder => F[Order] // algebra of domain behaviors/functions

  def execute(market: Market, brokerAccount: Account): Order => F[List[Execution]]

  def allocate(accounts: List[Account]): List[Execution] => F[List[Trade]]


  /*
  def tradeGeneration(t: Trading[F])(cor: ClientOrder, m: Market, bA: Account, l: List[Account]) = for {
    order <- t.fromClientOrder(cor)
    executions <- t.execute(m, bA)(order)
    trades <- t.allocate(l)(executions)
  } yield trades
  */

}

case class ClientOrder()
case class Order()
case class Market()
case class Account()
case class Execution()
case class Trade()


trait Logging

trait Auditing

trait TradeComponent[F[_]] extends Trading[F] with Logging with Auditing // modules aggregate into larger modules


// 1. Compositionality
// 2. Side-effects

// Side effects dont compose, violate modularity

// Algebra of types to the rescue

// Abstract side-effects into Data Type Constructors == Higher Kinded Types, which we call Effects


// SOME EFFECTS:
// List[A] non-determinism
// Option[A] partiality
// IO[A] external side-effects
// Either[A, B] disjunction
// Reader[E, A] read from environment aka dependency injection
// State[S, A] state management
// Writer[W, A] logging


// F[A] , A the answer tha the effect computes, F the additional stuff modeling the computation

// F[_] is an opaque type

// Just the algebra, no denotation, no concrete type, explicitly stating that we have effectful functions here