package org.waman.worldbrain.qkd.bb84.toy

import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.FixedBasesBobFactory
import org.waman.worldbrain.system.toy.{StateVector, StateBasis}
import spire.random.Generator

class Bob private (val keyLength: Int, rng: Generator)
    extends qkd.Bob{

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

object Bob extends FixedBasesBobFactory[Bob]{

  override protected def newBob(keyLength: Int, rng: Generator) =
    new Bob(keyLength, rng)
}