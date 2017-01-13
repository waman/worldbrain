package org.waman.worldbrain.single.b92

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.BasisVector._
import org.waman.worldbrain.single.Protocol.EstablishKey
import org.waman.worldbrain.single.Qubit
import spire.random.Generator

class Alice(protected val bitLength: Int)(implicit rng: Generator)
  extends worldbrain.Alice{

  require(bitLength > 0)

  private var bitString: Seq[Int] = _

  override val establishKeyBehavior: Receive = {
    case EstablishKey(bob) =>
      sendQubits(bob)

    case BitFilterMessage(filter) =>
      addKeyBits(extractKey(this.bitString, filter)) match {
        case x if x > 0 => sendQubits(sender())
        case _ =>
      }
  }

  private def sendQubits(bob: ActorRef): Unit = {
    this.bitString = createRandomBitString(this.bitLength, rng)

    val qubits = this.bitString.map{
      case 0 => Zero
      case 1 => Plus
    }.map(new Qubit(_))

    bob ! QubitMessage(qubits)
  }
}