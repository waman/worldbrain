package org.waman.worldbrain.qkd.bb84

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.waman.worldbrain.qkd.QkdProtocol.EstablishKey

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

object BB84App2 extends App{

  val keyLength = 18

  val system = ActorSystem("BB84System")
  implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
  implicit val timeout = Timeout(20 second)

  val alice = system.actorOf(Props(LoggingAlice[Double](keyLength, nChunk = 10)), s"Alice")
  val bob   = system.actorOf(Props(Bob[Double](keyLength)), s"Bob")

  alice ! EstablishKey(bob)

  val aliceProps = (alice ? RequestLog).mapTo[Map[String, Int]]

  Await.result(aliceProps, 60 second)
  aliceProps.foreach{ prop =>
    prop.foreach(println)
  }

  system.terminate()
}
