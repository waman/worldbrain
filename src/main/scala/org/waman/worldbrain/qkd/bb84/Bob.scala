package org.waman.worldbrain.qkd.bb84

import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.BobFactory
import org.waman.worldbrain.system.{StateBasis, StateVector}
import spire.math.Fractional
import spire.random.Generator

class Bob[A: Fractional] private (val keyLength: Int,
                                 protected val bases: Seq[StateBasis[A]],
                                 rng: Generator)
    extends qkd.Bob with StateEncoder[A]{

  private var basisSeq: Seq[StateBasis[A]] = _
  private var stateSeq: Seq[StateVector[A]] = _

  override val establishKeyBehavior: Receive = {
    case m: QubitMessage[A] =>
      this.basisSeq = createRandomBases(m.qubits.length)(rng)
      this.stateSeq = (m.qubits zip this.basisSeq).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }
      sender() ! RequestCorrectBases

    case CorrectBasisMessage(correctBases) =>
      val basisMatchingList =
        (this.basisSeq zip correctBases).map(x => if(x._1 == decodeBasis(x._2)) 1 else 0)

      addKeyBits(extractKey(this.stateSeq, basisMatchingList))
      sender() ! BasisFilterMessage(basisMatchingList)
  }
}

object Bob extends BobFactory{

  override protected def newBob[A](keyLength: Int,
                                   bases: Seq[StateBasis[A]],
                                   rng: Generator,
                                   a: Fractional[A]): Bob[A] =
    new Bob(keyLength, bases, rng)(a)
}