package org.waman.worldbrain.qkd.b92

import org.waman.worldbrain.qkd
import org.waman.worldbrain.system.single.BasisKet._
import org.waman.worldbrain.system.single.StateBasis._
import spire.random.Generator

class Bob(val keyLength: Int)(implicit rng: Generator)
    extends qkd.Bob{

  override val establishKeyBehavior: Receive = {
    case QubitMessage(qubits) =>
      val bitString = createRandomBitString(qubits.length, rng)

      val bitFilter = (qubits zip bitString).map{ case (qubit, bit) =>
        val basis = if(bit == 0) Hadamard else Standard
        qubit.observe(basis)(rng) match {
          case Zero | Plus  => 0
          case One  | Minus => 1
          case _ => throw new IllegalArgumentException("Unknow basis: "+basis)
        }
      }

      addKeyBits(extractKey(bitString, bitFilter))
      sender() ! BitFilterMessage(bitFilter)
  }
}