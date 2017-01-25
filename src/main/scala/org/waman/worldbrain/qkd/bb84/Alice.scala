package org.waman.worldbrain.qkd.bb84

import akka.actor.ActorRef
import akka.pattern._
import org.waman.worldbrain.Protocol.EstablishKey
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.Alice.RequestLog
import org.waman.worldbrain.system.single.BasisKet._
import org.waman.worldbrain.system.single._
import spire.random.Generator

import scala.concurrent.Promise

class Alice(val keyLength: Int, n0: Int, n1: Int, rng: Generator)
    extends qkd.Alice{

  private var states: Seq[BasisKet] = _

  //***** LOG *****
  private var createdQubitCount: Int = 0
  private var createdQubits: Seq[BasisKet] = Seq()
  private var currentFilter: Seq[Int] = Seq()
  private var usedQubitCount: Int = 0
  private val logPromise: Promise[Map[String, Any]] = Promise()
  //***************

  override val establishKeyBehavior: Receive = {
    case EstablishKey(bob) =>
      sendQubits(bob, n0)

    case RequestCorrectBases =>
      val bases = this.states.map(BasisKet.getBasis).map(encode)
      sender() ! CorrectBasisMessage(bases)

    case BasisFilterMessage(filter) =>
      //***** LOG *****
      // filter = Seq(0, 0, 1, 0, 1, 0)
      // this.currentKeyBitCount == 10
      // this.usedQubitCount == 15
      val scanned = filter.scan(this.currentKeyBitCount)(_ + _)
        // scanned = Seq(10, 10, 10, 11, 11, 12, 12)
      val i = scanned.indexOf(this.keyLength)  // this.keyLength == 11 => i == 3
      if(i >= 0) this.usedQubitCount += i  // this.usedQubitCount += 3
      else       this.usedQubitCount += filter.length
      this.currentFilter ++= filter
      //***************

      val key = extractKey(this.states, filter)
      addKeyBits(key) match {
        case x if x > 0 => sendQubits(sender(), n1)
        case _ =>
          //***** LOG *****
          val s = this.createdQubits.map(BasisKet.toSymbol).mkString
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
          //***************
      }

    case RequestLog =>
      //***** LOG *****
      import context.dispatcher
      pipe(logPromise.future) to sender()
    //***************
  }

  private def sendQubits(bob: ActorRef, n: Int): Unit = {
    this.states = (0 until n).map{ _ =>
      rng.nextInt(4) match {
        case 0 => Zero
        case 1 => One
        case 2 => Plus
        case 3 => Minus
      }
    }

    val qubits = this.states.map(new Qubit(_))

    //***** LOG *****
    this.createdQubits ++= this.states
    this.createdQubitCount += n
    //***************

    bob ! QubitMessage(qubits)
  }
}

object Alice {

  def apply(keyLength: Int,
            n0: Int = -1,
            n1: Int = -1,
            rng: Generator = Generator.rng): Alice = {
    new Alice(
      keyLength,
      if(n0 > 0) n0 else keyLength,
      if(n1 > 0) n1 else keyLength,
      rng
    )
  }
}