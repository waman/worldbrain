package org.waman.worldbrain.single.bb84

import org.waman.worldbrain
import org.waman.worldbrain.single.BasisVector._
import org.waman.worldbrain.single.Protocol.EstablishKey
import org.waman.worldbrain.single._
import spire.random.Generator

class Alice(implicit rng: Generator)
    extends worldbrain.Alice[Seq[Int]]{

  private var states: Seq[BasisVector] = _

  override val establishKeyBehavior: Receive = {
    case EstablishKey(bob, n) =>
      this.states = createRandomStates(n)
      val qubits = this.states.map(new Qubit(_))
      bob ! QubitMessage(qubits)

    case RequestCorrectBases =>
      val bases = this.states.map(BasisVector.getBasis).map(encode)
      sender() ! CorrectBasisMessage(bases)

    case BasisFilterMessage(filter) =>
      setKey(extractKey(this.states, filter))
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