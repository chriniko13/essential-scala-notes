package com.chriniko.essential.scala.chapter6_generating_random_data

object test {

  def main(args: Array[String]): Unit = {


    // ---
    // Markov Chain
    // Simple form: subject-verb-object , eg: Noel wrote code

    val subjects = List("Noel", "The cat", "The dog")
    val verbs = List("wrote", "chased", "slept on")
    val objects = List("the book", "the ball", "the bed")


    def allSentences(): Seq[(String, String, String)] = {
      subjects.flatMap(
        s => verbs.flatMap(
          v => objects.map(o => (s, v, o))
        )
      )
    }

    def allSentences2(): Seq[(String, String, String)] = {
      for {
        subject <- subjects
        verb <- verbs
        obj <- objects
      } yield (subject, verb, obj)
    }

    println
    println(allSentences().size)
    allSentences().foreach(println)

    println
    println(allSentences2().size)
    allSentences2().foreach(println)

    println

    val verbsBySubject = Map(
      "Noel" -> List("wrote", "chased", "slept on"),
      "The cat" -> List("meowed at", "chased", "slept on"),
      "The dog" -> List("barked at", "chased", "slept on")
    )

    val objectsByVerb = Map(
      "wrote" -> List("the book", "the letter", "the code"),
      "chased" -> List("the ball", "the dog", "the cat"),
      "slept on" -> List("the bed", "the mat", "the train"),
      "meowed at" -> List("Noel", "the door", "the food cupboard"),
      "barked at" -> List("the postman", "the car", "the cat")
    )

    def allSentencesDependent(): Seq[(String, String, String)] = {
      subjects.flatMap(
        subject => verbsBySubject(subject).flatMap(
          verb => objectsByVerb(verb).map(
            obj => (subject, verb, obj))
        )
      )
    }

    def allSentencesDependent2(): Seq[(String, String, String)] = {
      for {
        subject <- subjects
        verb <- verbsBySubject(subject)
        obj <- objectsByVerb(verb)
      } yield (subject, verb, obj)
    }

    println(allSentencesDependent().size)
    allSentencesDependent().foreach(println)

    println(allSentencesDependent2().size)
    allSentencesDependent2().foreach(println)


    // ---
    println


    sealed trait Coin
    case object Heads extends Coin
    case object Tails extends Coin

    val fairCoin: Distribution[Coin] = Distribution.uniform(List(Heads, Tails))


    val threeFlips = for {
      f1 <- fairCoin
      f2 <- fairCoin
      f3 <- fairCoin
    } yield (f1, f2, f3)

    println(threeFlips)


    // ---
    println


    // delicious smell --> 0.3 [*]
    // raw no smell --> 0.7 [*]
    // delicious smell, then cat harass me --> 0.8 (0.2 asleep) [*]
    // no smell, then cat harass hell of me --> 0.4 (0.6 asleep) [*]

    /*
      Implement this model and answer the question: if the cat comes to harass me
      what is the probability my food is producing delicious smells (and therefore is
      ready to eat.)
     */

    sealed trait Food
    case object Cooked extends Food
    case object Raw extends Food

    val food = Distribution.discrete(List(Cooked -> 0.3, Raw -> 0.7))


    sealed trait Cat
    case object Asleep extends Cat
    case object Harassing extends Cat

    def cat(f: Food): Distribution[Cat] = {
      f match {
        case Cooked => Distribution.discrete(List(Asleep -> 0.2, Harassing -> 0.8))
        case Raw => Distribution.discrete(List(Asleep -> 0.6, Harassing -> 0.4))
      }
    }

    val foodModel = for {
      f <- food
      c <- cat(f)
    } yield (f, c)

    println(foodModel)


  }

  // ---

  final case class Distribution[A](events: List[(A, Double)]) {

    def map[B](f: A => B): Distribution[B] = {
      val data = events.map(event => (f(event._1), event._2))
      Distribution(data)
    }

    def flatMap[B](f: A => Distribution[B]): Distribution[B] = {

      val data: List[(B, Double)] =
        events.flatMap(
          evt => f(evt._1).events.map(
            mappedEvt => mappedEvt._1 -> (evt._2 * mappedEvt._2)
          )
        )

      Distribution(data).compact.normalize
    }

    def normalize: Distribution[A] = {
      val totalWeight: Double = events.map(evt => evt._2).sum
      val data = events.map(evt => (evt._1, evt._2 / totalWeight))
      Distribution(data)
    }

    def compact: Distribution[A] = {
      val distinct = events.map(evt => evt._1).distinct

      def prob(a: A): Double = {
        events.filter(evt => evt._1 == a).map(evt => evt._2).sum
      }

      val data = distinct.map(evt => evt -> prob(evt))
      Distribution(data)
    }

  }

  object Distribution {

    def uniform[A](atoms: List[A]): Distribution[A] = {
      val p = 1.0 / atoms.length
      val data: List[(A, Double)] = atoms.map(r => (r, p))
      Distribution(data)
    }

    def discrete[A](events: List[(A,Double)]): Distribution[A] =
      Distribution(events).compact.normalize
  }

}
