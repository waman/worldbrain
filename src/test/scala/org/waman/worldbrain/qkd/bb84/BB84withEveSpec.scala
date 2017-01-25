package org.waman.worldbrain.qkd.bb84

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

case class Result(isSuccess: Int, matchingBitCount: Int){

  def +(other: Result): Result =
    Result(isSuccess+other.isSuccess, matchingBitCount+other.matchingBitCount)

  def toString(n: Int, bitLength: Int): String = {
    s"""$bitLength: ${1.0 - isSuccess.toFloat / n}
       |    ${matchingBitCount.toFloat / isSuccess}""".stripMargin
  }
}

class BB84withEveSpec extends WorldbrainCustomSpec{

  "Emulate BB84 with Eve" - {

    "with bitLength = 18, n = 2" in {
      execute(18, 2, "Quick")
    }

    "with bitLength = 1 to 10, n = 1000" in {
      (1 to 10).foreach {
        execute(_, 1000, "Slow")
      }
    }

    "with bitLength = 1 to 20, n = 10000" in {
      (1 to 20).foreach {
        execute(_, 10000, "Slower")
      }
    }

    def execute(bitLength: Int, n: Int, postfix: String): Unit = {

      val system = ActorSystem(s"BB84withEveSystem")
      implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
      implicit val timeout = Timeout(20 second)

      val successList: Seq[Future[Result]] = (0 until n).map{ i =>
        val alice = system.actorOf(Props(Alice(bitLength)), s"Alice$postfix$bitLength-$i")
        val bob   = system.actorOf(Props(new Bob(bitLength)), s"Bob$postfix$bitLength-$i")

        val bobe  = system.actorOf(Props(new Eve(alice, bob, bitLength)), s"Eve$bitLength-$i")

        alice ! EstablishKey(bobe)

        val aliceKey = (alice ? RequestKey).mapTo[Seq[Int]]
        val bobKey = (bob ? RequestKey).mapTo[Seq[Int]]
        val eveKey = (bobe ? RequestKey).mapTo[Seq[Int]]

        Future.sequence(Seq(aliceKey, bobKey, eveKey)).map{
          case aKey +: bKey +: eKey +: _ =>
            val result = if(aKey == bKey){
              Result(1, (aKey zip eKey).count(p => p._1 == p._2))
            } else{
              Result(0, 0)
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