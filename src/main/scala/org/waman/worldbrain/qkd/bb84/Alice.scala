package org.waman.worldbrain.qkd.bb84

import akka.actor.ActorRef
import org.waman.worldbrain.Protocol.EstablishKey
import org.waman.worldbrain.qkd
import org.waman.worldbrain.system.single.StateVector._
import org.waman.worldbrain.system.single._
import spire.random.Generator

class Alice(val keyLength: Int, n0: Int, n1: Int, rng: Generator)
    extends qkd.Alice{

  private var states: Seq[StateVector] = _

  override def establishKeyBehavior: Receive = {
    case EstablishKey(bob) =>
      sendQubits(bob, n0)

    case RequestCorrectBases =>
      val bases = this.states.map(StateVector.getBasis).map(encode)
      sender() ! CorrectBasisMessage(bases)

    case BasisFilterMessage(filter) =>
      sendBasisFilterMessage(filter)
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

    qubitsCreated(this.states)

    bob ! QubitMessage(qubits)
  }

  protected def qubitsCreated(states: Seq[StateVector]): Unit = {}

  protected def sendBasisFilterMessage(filter: Seq[Int]): Unit = {
    val key = extractKey(this.states, filter)
    addKeyBits(key) match {
      case x if x > 0 => sendQubits(sender(), n1)
      case _ =>
        keyEstablished()
    }
  }

  protected def keyEstablished(): Unit = {}
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