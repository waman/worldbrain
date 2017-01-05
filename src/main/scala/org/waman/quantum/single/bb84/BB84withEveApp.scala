package org.waman.quantum.single.bb84

import akka.Done
import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.waman.quantum.KeyContainer.RequestKey
import org.waman.quantum.single.bb84.BB84.EstablishKey

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

case class Result(isSuccess: Int, matchingBitCount: Int){

  def +(other: Result): Result =
    Result(isSuccess+other.isSuccess, matchingBitCount+other.matchingBitCount)

  def toString(n: Int, bitLength: Int): String = {
    s"""$bitLength: ${1.0 - isSuccess.toFloat / n}
       |    ${matchingBitCount.toFloat / isSuccess}""".stripMargin
  }
}

object BB84withEveApp extends App{

  val system = ActorSystem("BB84withEveSystem")
  implicit val dispatcher = system.dispatcher
  implicit val timeout = Timeout(60 second)

  val n = 1000
  def exec(bitLength: Int): Unit = {
    val successList: Seq[Future[Result]] = (0 until n).map{ i =>
      val alice = system.actorOf(Props[Alice], s"Alice$bitLength-$i")
      val bob   = system.actorOf(Props[Bob], s"Bob$bitLength-$i")

      val bobe  = system.actorOf(Props(new Eve(alice, bob)), s"Eve$bitLength-$i")

      alice ! EstablishKey(bobe, bitLength)

      val aliceKey = (alice ? RequestKey).mapTo[Seq[Int]]
      val bobKey = (bob ? RequestKey).mapTo[Seq[Int]]
      val eveKey = (bobe ? RequestKey).mapTo[Seq[Int]]

      Future.sequence(Seq(aliceKey, bobKey, eveKey)).map{
        case aKey +: bKey +: eKey +: _ =>
          val result = if(aKey.nonEmpty && aKey == bKey){
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

  val maxBitLength = 20
  try {
    (10 to maxBitLength).foreach(exec)

  }finally {
    system.terminate()
  }
}