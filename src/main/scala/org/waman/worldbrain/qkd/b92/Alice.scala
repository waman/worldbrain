package org.waman.worldbrain.qkd.b92

import akka.actor.{Actor, ActorRef}
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.AliceFactory
import org.waman.worldbrain.qkd.QkdProtocol.EstablishKey
import org.waman.worldbrain.system.{Qubit, StateBasis}
import spire.math._
import spire.random.Generator

class Alice[A: Fractional] private (val keyLength: Int,
                                    protected val bases: Seq[StateBasis[A]],
                                    nChunk: Int,
                                    rng: Generator)
    extends qkd.Alice with StateAlias[A]{

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
    this.bitString = createRandomBitString(nChunk)(rng)

    val qubits = this.bitString.map{
      case 0 => zero
      case 1 => plus
    }.map(Qubit(_))

    bob ! QubitMessage(qubits)
  }
}

object Alice extends AliceFactory{

  override protected def newAlice[A](keyLength: Int, bases: Seq[StateBasis[A]], nChunk: Int,
                                     rng: Generator, a: Fractional[A]): Alice[A] =
    new Alice(keyLength, bases, nChunk, rng)(a)
}