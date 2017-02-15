package org.waman.worldbrain.qkd.bb84

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.AliceFactory
import org.waman.worldbrain.qkd.QkdProtocol.EstablishKey
import org.waman.worldbrain.system.single._
import spire.math.Fractional
import spire.random.Generator

class Alice[A: Fractional] protected (val keyLength: Int,
                                      protected val bases: Seq[StateBasis[A]],
                                      nChunk: Int,
                                      rng: Generator)
    extends qkd.Alice with StateEncoder[A]{

  private var states: Seq[BasisVector[A]] = _

  override def establishKeyBehavior: Receive = {
    case EstablishKey(bob) =>
      sendQubits(bob)

    case RequestCorrectBases =>
      val bases = this.states.map(getBasis).map(encodeBasis)
      sender() ! CorrectBasisMessage(bases)

    case BasisFilterMessage(filter) =>
      sendBasisFilterMessage(filter)
  }

  private def sendQubits(bob: ActorRef): Unit = {
    this.states = (0 until nChunk).map{ _ =>
      rng.nextInt(4) match {
        case 0 => zero
        case 1 => one
        case 2 => plus
        case 3 => minus
      }
    }

    val qubits = this.states.map(new Qubit[A](_))

    qubitsCreated(this.states)

    bob ! QubitMessage(qubits)
  }

  /** For logging */
  protected def qubitsCreated(states: Seq[BasisVector[A]]): Unit = {}

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

object Alice extends AliceFactory{

  override protected def newAlice[A](keyLength: Int,
                                     bases: Seq[StateBasis[A]],
                                     nChunk: Int,
                                     rng: Generator,
                                     a: Fractional[A]): Alice[A] =
    new Alice(keyLength, bases, nChunk, rng)(a)
}