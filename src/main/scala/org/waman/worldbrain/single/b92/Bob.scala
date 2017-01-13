package org.waman.worldbrain.single.b92

import org.waman.worldbrain
import org.waman.worldbrain.single.BasisVector._
import org.waman.worldbrain.single.StateBasis._
import spire.random.Generator

class Bob(protected val bitLength: Int)(implicit rng: Generator)
    extends worldbrain.Bob{

  override val establishKeyBehavior: Receive = {
    case QubitMessage(qubits) =>
      val bitString = createRandomBitString(qubits.length, rng)

      val bitFilter = (qubits zip bitString).map{ case (qubit, bit) =>
        val basis = if(bit == 0) Hadamard else Standard
        qubit.observe(basis)(rng) match {
          case Zero | Plus  => 0
          case One  | Minus => 1
        }
      }

      addKeyBits(extractKey(bitString, bitFilter))
      sender() ! BitFilterMessage(bitFilter)
  }
}