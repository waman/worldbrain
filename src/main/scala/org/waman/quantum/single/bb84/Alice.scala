package org.waman.quantum.single.bb84

import org.waman.quantum
import org.waman.quantum.Random._
import org.waman.quantum.single.BasisVector._
import org.waman.quantum.single.bb84.BB84._
import org.waman.quantum.single.{BasisVector, Qubit}

class Alice extends quantum.Alice[Seq[Int]]{

  private var states: Seq[BasisVector] = _

  override val establishKeyBehavior: Receive = {
    case EstablishKey(bob, n) =>
      this.states = createRandomStates(n)
      val qubits = this.states.map(new Qubit(_))
      bob ! QubitMessage(qubits)

    case RequestCorrectBases =>
      val bases = this.states.map(BasisVector.getBasis)
      sender() ! CorrectBasisMessage(bases)

    case BasisFilterMessage(filter) =>
      setKey(extractKey(this.states, filter))
  }

  private def createRandomStates(n: Int): Seq[BasisVector] = {
    (0 until n).map{i =>
      nextInt(4) match {
        case 0 => Zero
        case 1 => One
        case 2 => Plus
        case 3 => Minus
      }
    }
  }
}