package org.waman.quantum.single.bb84

import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.waman.quantum.KeyContainer.RequestKey
import org.waman.quantum.single.bb84.BB84.EstablishKey

import scala.concurrent.{Await, Future}

object BB84App extends App{

  val system = ActorSystem("BB84System")
  implicit val dispatcher = system.dispatcher
  implicit val timeout = Timeout(60 second)

  val bitSize = 20
  val n = 1000
  val lengthList: Seq[Future[Int]] = (0 until n).map{ i =>
    val alice = system.actorOf(Props[Alice], "Alice"+i)
    val bob   = system.actorOf(Props[Bob], "Bob"+i)

    alice ! EstablishKey(bob, bitSize)

    val aliceKey = (alice ? RequestKey).mapTo[Seq[Int]]
    val bobKey = (bob ? RequestKey).mapTo[Seq[Int]]

    aliceKey.zip(bobKey).map{ keys =>
      assert(keys._1 == keys._2)
      val key = keys._1
      println(s"$i. ${key.mkString}: $alice <-> $bob")
      key.length
    }
  }

  val lengthSum: Future[Int] = Future.sequence(lengthList).map(_.sum)
  Await.result(lengthSum, 60 second)
  lengthSum.foreach(sum => println(sum.toDouble / n))
  system.terminate()
}