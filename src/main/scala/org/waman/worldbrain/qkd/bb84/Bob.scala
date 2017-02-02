package org.waman.worldbrain.qkd.bb84

import org.waman.worldbrain.qkd
import org.waman.worldbrain.system.single.{StateBasis, StateVector}
import spire.random.Generator

class Bob(val keyLength: Int)
         (implicit rng: Generator) extends qkd.Bob{

  require(keyLength > 0)

  private var bases: Seq[StateBasis] = _
  private var states: Seq[StateVector] = _

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

      addKeyBits(extractKey(this.states, basisMatchingList))
      sender() ! BasisFilterMessage(basisMatchingList)
  }
}