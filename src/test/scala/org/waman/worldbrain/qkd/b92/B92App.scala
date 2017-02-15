package org.waman.worldbrain.qkd.b92

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.waman.worldbrain.qkd.QkdProtocol.{EstablishKey, RequestKey}

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

object B92App extends App{

  val bitLength = 20
  val nSample = 1000

  val system = ActorSystem(s"B92System")
  implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
  implicit val timeout = Timeout(20 second)

  val lengthList: Seq[Future[Int]] = (0 until nSample).map{ i =>
    val alice = system.actorOf(Props(Alice[Double](bitLength)), s"Alice$i")
    val bob   = system.actorOf(Props(Bob[Double](bitLength)), s"Bob$i")

    alice ! EstablishKey(bob)

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
  lengthSum.foreach(sum => println(sum.toDouble / nSample))

  system.terminate()
}
