package org.waman.worldbrain.qkd.bb84

import akka.pattern.pipe
import org.waman.worldbrain.qkd.bb84.LoggingAlice.RequestLog
import org.waman.worldbrain.system.single.StateVector
import spire.random.Generator

import scala.concurrent.Promise

class LoggingAlice(keyLength: Int, n0: Int, n1: Int, rng: Generator)
    extends Alice(keyLength, n0, n1, rng){

  private var createdQubitCount: Int = 0
  private var createdQubits: Seq[StateVector] = Seq()
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

  override protected def qubitsCreated(states: Seq[StateVector]) = {
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
    val s = this.createdQubits.map(_.symbol).mkString
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

object LoggingAlice {

  case object RequestLog

  def apply(keyLength: Int,
            n0: Int = -1,
            n1: Int = -1,
            rng: Generator = Generator.rng): LoggingAlice = {
    new LoggingAlice(
      keyLength,
      if(n0 > 0) n0 else keyLength,
      if(n1 > 0) n1 else keyLength,
      rng
    )
  }
}