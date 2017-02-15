package org.waman.worldbrain.qkd.b92.toy

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.QkdProtocol.EstablishKey
import org.waman.worldbrain.qkd.FixedBasesAliceFactory
import org.waman.worldbrain.system.single.toy.Qubit
import org.waman.worldbrain.system.single.toy.BasisVector._
import spire.random.Generator

class Alice private(val keyLength: Int, nChunk: Int, rng: Generator)
  extends qkd.Alice{

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

object Alice extends FixedBasesAliceFactory[Alice]{

  override protected def newAlice(keyLength: Int, nChunk: Int, rng: Generator) =
    new Alice(keyLength, nChunk, rng)
}