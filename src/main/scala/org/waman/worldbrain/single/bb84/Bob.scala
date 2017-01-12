package org.waman.worldbrain.single.bb84

import org.waman.worldbrain
import org.waman.worldbrain.single.{BasisVector, StateBasis}
import spire.random.Generator

class Bob(implicit rng: Generator) extends worldbrain.Bob[Seq[Int]]{

  private var bases: Seq[StateBasis] = _
  private var states: Seq[BasisVector] = _

  override val establishKeyBehavior: Receive = {
    case QubitMessage(qubits) =>
      this.bases = createRandomBases(rng, qubits.length)
      this.states = (qubits zip this.bases).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }
      sender() ! RequestCorrectBases

    case CorrectBasisMessage(correctBases) =>
      val basisMatchingList =
        (this.bases zip correctBases).map(x => if(x._1 == decode(x._2)) 1 else 0)

      setKey(extractKey(this.states, basisMatchingList))
      sender() ! BasisFilterMessage(basisMatchingList)
  }
}