package org.waman.worldbrain.single.bb84

import org.waman.worldbrain
import org.waman.worldbrain.single.{BasisVector, StateBasis}
import spire.random.Generator

class Bob(protected val bitLength: Int)
         (implicit rng: Generator) extends worldbrain.Bob{

  require(bitLength > 0)

  private var bases: Seq[StateBasis] = _
  private var states: Seq[BasisVector] = _

  override val establishKeyBehavior: Receive = {
    case QubitMessage(qubits) =>
      this.bases = StateBasis.createRandomBases(rng, qubits.length)
      this.states = (qubits zip this.bases).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }
      sender() ! RequestCorrectBases

    case CorrectBasisMessage(correctBases) =>
      val basisMatchingList =
        (this.bases zip correctBases).map(x => if(x._1 == decode(x._2)) 1 else 0)

      addKeyBits(extractKey(this.states, basisMatchingList))
      sender() ! BasisFilterMessage(basisMatchingList)
  }
}