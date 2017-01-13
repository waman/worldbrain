package org.waman.worldbrain.single.bb84

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.BasisVector._
import org.waman.worldbrain.single.Protocol.EstablishKey
import org.waman.worldbrain.single._
import spire.random.Generator

import scala.collection.mutable

class Alice(protected val bitLength: Int)(implicit rng: Generator)
    extends worldbrain.Alice{

  require(bitLength > 0)

  private var states: Seq[BasisVector] = _
  private val key: mutable.Seq[Int] = mutable.Seq()

  override val establishKeyBehavior: Receive = {
    case EstablishKey(bob) =>
      sendQubits(bob)

    case RequestCorrectBases =>
      val bases = this.states.map(BasisVector.getBasis).map(encode)
      sender() ! CorrectBasisMessage(bases)

    case BasisFilterMessage(filter) =>
      val key = extractKey(this.states, filter)
      addKeyBits(key) match {
        case x if x > 0 => sendQubits(sender())
        case _ =>
      }
  }

  private def sendQubits(bob: ActorRef): Unit = {
    this.states = createRandomStates(this.bitLength)
    val qubits = this.states.map(new Qubit(_))
    bob ! QubitMessage(qubits)
  }

  private def createRandomStates(n: Int): Seq[BasisVector] = {
    (0 until n).map{ _ =>
      rng.nextInt(4) match {
        case 0 => Zero
        case 1 => One
        case 2 => Plus
        case 3 => Minus
      }
    }
  }
}