package org.waman.worldbrain.qkd.bb84

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.waman.worldbrain.qkd.QkdProtocol.{EstablishKey, RequestKey}

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

object BB84App1 extends App{

  val keyLength = 20
  val nSample = 1000

  val system = ActorSystem(s"BB84System")
  implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
  implicit val timeout = Timeout(20 second)

  val lengthList: Seq[Future[Int]] = (0 until nSample).map{ i =>
    val alice = system.actorOf(Props(Alice[Double](keyLength)), s"Alice$i")
    val bob   = system.actorOf(Props(Bob[Double](keyLength)), s"Bob$i")

    alice ! EstablishKey(bob)

    val aliceKey = (alice ? RequestKey).mapTo[Seq[Int]]
    val bobKey = (bob ? RequestKey).mapTo[Seq[Int]]

    (aliceKey zip bobKey).map{ keys =>
      assert(keys._1 == keys._2)
      val key = keys._1
      println(s"$i. ${key.mkString}: $alice <-> $bob")
      key.length
    }
  }

  val lengthSum: Future[Int] = Future.sequence(lengthList).map(_.sum)
  Await.result(lengthSum, 60 second)
  lengthSum.foreach { sum =>
    assert( sum / nSample == keyLength )
    println(sum.toFloat / nSample)
  }

  system.terminate()
}
