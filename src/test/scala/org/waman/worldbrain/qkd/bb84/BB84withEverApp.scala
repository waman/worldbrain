package org.waman.worldbrain.qkd.bb84

import akka.Done
import akka.actor.{ActorSystem, Props}
import akka.pattern._
import akka.util.Timeout
import org.waman.worldbrain.qkd.QkdProtocol.{EstablishKey, RequestKey}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.language.postfixOps
import spire.implicits._

object BB84withEverApp extends App{

  val maxBitLength = 20
  val nSample = 1000

  val system = ActorSystem(s"BB84withEverSystem")
  implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
  implicit val timeout = Timeout(20 second)

  def execute(bitLength: Int): Unit = {
    val successList: Seq[Future[Result]] = (0 until nSample).map { i =>
      val alice = system.actorOf(Props(Alice[Double](bitLength)), s"Alice$bitLength-$i")
      val bob = system.actorOf(Props(Bob[Double](bitLength)), s"Bob$bitLength-$i")

      val bobe = system.actorOf(Props(Ever[Double](alice, bob, bitLength)), s"Ever$bitLength-$i")

      alice ! EstablishKey(bobe)

      val aliceKey = (alice ? RequestKey).mapTo[Seq[Int]]
      val bobKey = (bob ? RequestKey).mapTo[Seq[Int]]
      val eveKey = (bobe ? RequestKey).mapTo[Seq[Int]]

      Future.sequence(Seq(aliceKey, bobKey, eveKey)).map {
        case aKey +: bKey +: eKey +: _ =>
          val result = if (aKey == bKey)
            Result(1, (aKey zip eKey).count(p => p._1 == p._2))
          else
            Result(0, 0)

          alice ! Done
          bob ! Done
          bobe ! Done

          result
      }
    }

    val success: Future[Result] = Future.sequence(successList).map(_.reduce(_ + _))

    Await.result(success, 60 second)
    success.foreach(r => println(r.toString(nSample, bitLength)))
  }

  (1 to maxBitLength).foreach(execute)
  system.terminate()
}
