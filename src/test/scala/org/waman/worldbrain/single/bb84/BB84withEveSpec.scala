package org.waman.worldbrain.single.bb84

import akka.Done
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import akka.pattern._

import org.waman.worldbrain.KeyContainer.RequestKey
import org.waman.worldbrain.WorldbrainCustomSpec
import org.waman.worldbrain.single.bb84.BB84._

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

class BB84withEveSpec extends WorldbrainCustomSpec{

  val system = ActorSystem("BB84withEveSystem")
  implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
  implicit val timeout = Timeout(180 second)

  "execute" - {

    def execute(bitLength: Int, n: Int): Unit = {
      val successList: Seq[Future[Result]] = (0 until n).map{ i =>
        val alice = system.actorOf(Props[Alice], s"Alice$bitLength-$i")
        val bob   = system.actorOf(Props[Bob], s"Bob$bitLength-$i")

        val bobe  = system.actorOf(Props(new Ever(alice, bob)), s"Ever$bitLength-$i")

        alice ! EstablishKey(bobe, bitLength)

        val aliceKey = (alice ? RequestKey).mapTo[Seq[Int]]
        val bobKey = (bob ? RequestKey).mapTo[Seq[Int]]
        val eveKey = (bobe ? RequestKey).mapTo[Seq[Int]]

        Future.sequence(Seq(aliceKey, bobKey, eveKey)).map{
          case aKey +: bKey +: eKey +: _ =>
            val result = if(aKey.nonEmpty && aKey == bKey)
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
      Await.result(success, 180 second)
      success.foreach(r => println(r.toString(n, bitLength)))
    }

    "test" in {
      try {
        val minBitLength = 10
        val maxBitLength = 20
        val n = 1000
        (minBitLength to maxBitLength).foreach(execute(_, n))

      }finally {
        system.terminate()
      }
    }
  }
}
