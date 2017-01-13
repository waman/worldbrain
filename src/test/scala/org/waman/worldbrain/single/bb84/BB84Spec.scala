package org.waman.worldbrain.single.bb84

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.waman.worldbrain.KeyContainer.RequestKey
import org.waman.worldbrain.WorldbrainCustomSpec
import org.waman.worldbrain.single.Protocol.EstablishKey

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

class BB84Spec extends WorldbrainCustomSpec{

  "Emulate BB84 key distribution protocol" - {

    "with bitSize = 18, n = 2" in {
      executeWith(18, 2, "Quick")
    }

    "with bitSize = 20, n = 1000" in {
      executeWith(20, 1000, "Slow")
    }

    def executeWith(bitLength: Int, n: Int, postfix: String): Unit = {

      val system = ActorSystem(s"BB84${postfix}System")
      implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
      implicit val timeout = Timeout(20 second)

      val lengthList: Seq[Future[Int]] = (0 until n).map{ i =>
        val alice = system.actorOf(Props(new Alice(bitLength)), s"Alice$postfix$i")
        val bob   = system.actorOf(Props(new Bob(bitLength)), s"Bob$postfix$i")

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
      lengthSum.foreach(sum => println(sum.toDouble / n))
    }
  }
}
