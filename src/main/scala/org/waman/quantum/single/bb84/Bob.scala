package org.waman.quantum.single.bb84

import org.waman.quantum
import org.waman.quantum.single.bb84.BB84._
import org.waman.quantum.single.{BasisVector, StateBasis}

class Bob extends quantum.Bob[Seq[Int]]{

  private var bases: Seq[StateBasis] = _
  private var states: Seq[BasisVector] = _

  override val establishKeyBehavior: Receive = {
    case QubitMessage(qubits) =>
      this.bases = createRandomBases(qubits.length)
      this.states = qubits.zip(this.bases).map{
        case (qubit, basis) => qubit.observe(basis)
      }
      sender() ! RequestCorrectBases

    case CorrectBasisMessage(correctBases) =>
      val basisMatchingList = this.bases.zip(correctBases).map(x => x._1 == x._2)
      setKey(extractKey(this.states, basisMatchingList))
      sender() ! BasisFilterMessage(basisMatchingList)
  }
}