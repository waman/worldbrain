package org.waman.worldbrain.qkd.b92

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.Protocol.EstablishKey
import org.waman.worldbrain.system.single.StateVector._
import org.waman.worldbrain.system.single.Qubit
import spire.random.Generator

class Alice(val keyLength: Int)(implicit rng: Generator)
  extends qkd.Alice{

  require(keyLength > 0)

  private var bitString: Seq[Int] = _

  override val establishKeyBehavior = {
    case EstablishKey(bob) =>
      sendQubits(bob)

    case BitFilterMessage(filter) =>
      addKeyBits(extractKey(this.bitString, filter)) match {
        case x if x > 0 => sendQubits(sender())
        case _ =>
      }
  }

  private def sendQubits(bob: ActorRef): Unit = {
    this.bitString = createRandomBitString(this.keyLength, rng)

    val qubits = this.bitString.map{
      case 0 => Zero
      case 1 => Plus
    }.map(new Qubit(_))

    bob ! QubitMessage(qubits)
  }
}