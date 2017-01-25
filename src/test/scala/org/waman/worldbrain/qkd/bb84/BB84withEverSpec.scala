package org.waman.worldbrain.qkd.bb84

import akka.Done
import akka.actor.{ActorSystem, Props}
import akka.pattern._
import akka.util.Timeout
import org.waman.worldbrain.qkd.KeyContainer.RequestKey
import org.waman.worldbrain.WorldbrainCustomSpec
import org.waman.worldbrain.Protocol.EstablishKey
import spire.random.Generator

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.language.postfixOps

class BB84withEverSpec extends WorldbrainCustomSpec{

  val rng = new RandomInRealBasisFactory(Generator.rng)

  "Emulate BB84 key distribution protocol eavesdropped by Eve" - {

    "with bitLength = 18, n = 2, and random bases" in {
      execute(18, 2, "Quick", rng)
    }

    "with bitLength 1 to 10, n = 1000 and random bases" in {
      (1 to 10).foreach{
        execute(_, 1000, "Slow", rng)
      }
    }

    "with bitLength 1 to 10, n = 1000 and fixed basis (theta = 0)" in {
      (1 to 10).foreach{
        execute(_, 1000, "theta0", new FixedBasisFactory(0.0))
      }
    }

    "with bitLength 1 to 10, n = 1000 and fixed basis (theta = PI)" in {
      (10 to 20).foreach{
        execute(_, 1000, "thetaPI", new FixedBasisFactory(Math.PI))
      }
    }

    def execute(bitLength: Int, n: Int, postfix: String, bf: BasisFactory): Unit = {
      val system = ActorSystem(s"BB84withEverSystem")
      implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
      implicit val timeout = Timeout(20 second)

      val successList: Seq[Future[Result]] = (0 until n).map{ i =>
        val alice = system.actorOf(Props(Alice(bitLength)), s"Alice$postfix$bitLength-$i")
        val bob   = system.actorOf(Props(new Bob(bitLength)), s"Bob$postfix$bitLength-$i")

        val bobe  = system.actorOf(Props(Ever(alice, bob, bitLength)), s"Ever$postfix$bitLength-$i")

        alice ! EstablishKey(bobe)

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

      Await.result(success, 60 second)
      success.foreach(r => println(r.toString(n, bitLength)))
    }
  }
}