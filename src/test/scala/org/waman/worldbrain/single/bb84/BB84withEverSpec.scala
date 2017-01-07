package org.waman.worldbrain.single.bb84

import akka.Done
import akka.actor.{ActorSystem, Props}
import akka.pattern._
import akka.util.Timeout
import org.scalatest.BeforeAndAfter
import org.waman.worldbrain.KeyContainer.RequestKey
import org.waman.worldbrain.WorldbrainCustomSpec
import org.waman.worldbrain.single.bb84.BB84._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.language.postfixOps

class BB84withEverSpec extends WorldbrainCustomSpec with BeforeAndAfter{

  val system = ActorSystem("BB84withEveSystem")
  implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
  implicit val timeout = Timeout(180 second)

  "Emulate BB84 key distribution protocol eavesdropped by Eve" - {

    "with bit length 18 and n = 1" in {
      execute(18, 1, "Quick")
    }

    "with bit length 10 to 20 and n = 1000" in {
      val minBitLength = 10
      val maxBitLength = 20
      val n = 1000
      (minBitLength to maxBitLength).foreach(execute(_, n, "Slow"))
    }

    def execute(bitLength: Int, n: Int, postfix: String): Unit = {
      val successList: Seq[Future[Result]] = (0 until n).map{ i =>
        val alice = system.actorOf(Props(new Alice), s"Alice$postfix$bitLength-$i")
        val bob   = system.actorOf(Props(new Bob), s"Bob$postfix$bitLength-$i")

        val bobe  = system.actorOf(Props(new Ever(alice, bob)), s"Ever$postfix$bitLength-$i")

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
  }

  after{
    system.terminate()
  }
}
