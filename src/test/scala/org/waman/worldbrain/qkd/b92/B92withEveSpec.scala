package org.waman.worldbrain.qkd.b92

import akka.Done
import akka.actor._
import akka.pattern._
import akka.util.Timeout
import org.waman.worldbrain.qkd.KeyContainer.RequestKey
import org.waman.worldbrain.WorldbrainCustomSpec
import org.waman.worldbrain.Protocol.EstablishKey

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

case class Result(successCount: Int, length: Int, matchingBitCount: Int){

  def +(other: Result): Result =
    Result(
      successCount + other.successCount,
      length + other.length,
      matchingBitCount + other.matchingBitCount)

  def toString(n: Int, bitLength: Int): String = {
    s"""$bitLength: ${1.0 - successCount.toFloat / n}
       |    ${length.toFloat / successCount}
       |    ${matchingBitCount.toFloat / successCount}""".stripMargin
  }
}

class BB84withEveSpec extends WorldbrainCustomSpec{

  "Emulate B92 with Eve" - {

    "with bitLength = 18, n = 2" in {
      execute(18, 2, "Quick")
    }

    "with bitLength = 1 to 10, n = 1000" in {
      (1 to 10).foreach{ i =>
        execute(i, 1000, "Slow")
      }
    }

    def execute(bitLength: Int, n: Int, postfix: String): Unit = {

      val system = ActorSystem(s"B92withEveSystem")
      implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
      implicit val timeout = Timeout(20 second)

      val successList: Seq[Future[Result]] = (0 until n).map{ i =>
        val alice = system.actorOf(Props(new Alice(bitLength)), s"Alice$postfix$bitLength-$i")
        val bob   = system.actorOf(Props(new Bob(bitLength)), s"Bob$postfix$bitLength-$i")

        val bobe  = system.actorOf(Props(new Eve(alice, bob, bitLength)), s"Eve$bitLength-$i")

        alice ! EstablishKey(bobe)

        val aliceKey = (alice ? RequestKey).mapTo[Seq[Int]]
        val bobKey = (bob ? RequestKey).mapTo[Seq[Int]]
        val eveKey = (bobe ? RequestKey).mapTo[Seq[Int]]

        Future.sequence(Seq(aliceKey, bobKey, eveKey)).map{
          case aKey +: bKey +: eKey +: _ =>
            val result = if(aKey == bKey){
              Result(1, aKey.length, (aKey zip eKey).count(p => p._1 == p._2))
            } else{
              Result(0, 0, 0)
            }

            alice ! Done
            bob ! Done
            bobe ! Done

            result
        }
      }

      val success: Future[Result] = Future.sequence(successList).map(_.reduce(_ + _))
      Await.result(success, 60 second)
      success.foreach(r => println(r.toString(n, bitLength)))
    }
  }
}
