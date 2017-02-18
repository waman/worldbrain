package org.waman.worldbrain.qkd.bb84.toy

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.FixedBasesAliceFactory
import org.waman.worldbrain.qkd.QkdProtocol.EstablishKey
import org.waman.worldbrain.system.single.toy.StateVector._
import org.waman.worldbrain.system.single.toy.{StateVector, Qubit}
import spire.random.Generator

class Alice private (val keyLength: Int, nChunk: Int, rng: Generator)
    extends qkd.Alice with StateEncoder{

  private var states: Seq[StateVector] = _

  override def establishKeyBehavior: Receive = {
    case EstablishKey(bob) =>
      sendQubits(bob)

    case RequestCorrectBases =>
      val bases = this.states.map(getBasis).map(encode)
      sender() ! CorrectBasisMessage(bases)

    case BasisFilterMessage(filter) =>
      sendBasisFilterMessage(filter)
  }

  private def sendQubits(bob: ActorRef): Unit = {
    this.states = (0 until nChunk).map{ _ =>
      rng.nextInt(4) match {
        case 0 => Zero
        case 1 => One
        case 2 => Plus
        case 3 => Minus
      }
    }

    val qubits = this.states.map(new Qubit(_))

    qubitsCreated(this.states)

    bob ! QubitMessage(qubits)
  }

  /** For logging */
  protected def qubitsCreated(states: Seq[StateVector]): Unit = {}

  /** For logging */
  protected def sendBasisFilterMessage(filter: Seq[Int]): Unit = {
    val key = extractKey(this.states, filter)
    addKeyBits(key) match {
      case x if x > 0 => sendQubits(sender())
      case _          => keyEstablished()
    }
  }

  /** For logging */
  protected def keyEstablished(): Unit = {}
}

object Alice extends FixedBasesAliceFactory[Alice]{

  override protected def newAlice(keyLength: Int, nChunk: Int, rng: Generator) =
    new Alice(keyLength, nChunk, rng)
}