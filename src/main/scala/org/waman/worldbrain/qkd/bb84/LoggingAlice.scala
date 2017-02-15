package org.waman.worldbrain.qkd.bb84

import akka.actor.Actor
import akka.pattern.pipe
import org.waman.worldbrain.qkd.AliceFactory
import org.waman.worldbrain.system.single.{BasisVector, StateBasis, StateSpace}
import spire.math.Fractional
import spire.random.Generator

import scala.concurrent.Promise

class LoggingAlice[A: Fractional] private(keyLength: Int,
                                          bases: Seq[StateBasis[A]],
                                          nChunk: Int,
                                          rng: Generator)
    extends Alice[A](keyLength, bases, nChunk, rng){

  private var createdQubitCount: Int = 0
  private var createdQubits: Seq[BasisVector[A]] = Seq()
  private var currentFilter: Seq[Int] = Seq()
  private var usedQubitCount: Int = 0
  private val logPromise: Promise[Map[String, Any]] = Promise()


  override def establishKeyBehavior =
    getLogBehavior.orElse[Any, Unit](super.establishKeyBehavior)

  def getLogBehavior: Receive = {
    case RequestLog =>
      import context.dispatcher
      pipe(logPromise.future) to sender()
  }

  override protected def qubitsCreated(states: Seq[BasisVector[A]]): Unit = {
    this.createdQubits ++= states
    this.createdQubitCount += states.length
  }

  override protected def sendBasisFilterMessage(filter: Seq[Int]) = {
    // filter = Seq(0, 0, 1, 0, 1, 0)
    // this.currentKeyBitCount == 10
    // this.usedQubitCount == 15
    val scanned = filter.scan(this.currentKeyBitCount)(_ + _)
    // scanned = Seq(10, 10, 10, 11, 11, 12, 12)
    val i = scanned.indexOf(this.keyLength)  // this.keyLength == 11 => i == 3
    if(i >= 0) this.usedQubitCount += i  // this.usedQubitCount += 3
    else       this.usedQubitCount += filter.length
    this.currentFilter ++= filter

    super.sendBasisFilterMessage(filter)
  }

  override protected def keyEstablished() = {
    val s = this.createdQubits.map{
      case v if v == zero  => "0"
      case v if v == one   => "1"
      case v if v == plus  => "+"
      case v if v == minus => "-"
    }.mkString

    this.logPromise.success(
      Map(
        "keyLength        " -> this.keyLength,
        "createdQubitCount" -> this.createdQubitCount,
        "createdQubits    " -> s,
        "filter           " -> this.currentFilter.mkString,
        "usedQubitCount   " -> this.usedQubitCount,
        "usedQubits       " -> s.substring(0, this.usedQubitCount)
      )
    )
  }
}

object LoggingAlice extends AliceFactory{

  override protected def newAlice[A](keyLength: Int,
                                     bases: Seq[StateBasis[A]],
                                     nChunk: Int,
                                     rng: Generator,
                                     a: Fractional[A]): LoggingAlice[A] =
    new LoggingAlice(keyLength, bases, nChunk, rng)(a)
}