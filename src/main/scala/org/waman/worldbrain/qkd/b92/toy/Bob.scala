package org.waman.worldbrain.qkd.b92.toy

import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.FixedBasesBobFactory
import org.waman.worldbrain.system.toy.StateBasis._
import org.waman.worldbrain.system.toy.StateVector._
import spire.random.Generator

class Bob private (val keyLength: Int, rng: Generator)
    extends qkd.Bob{

  override val establishKeyBehavior: Receive = {
    case QubitMessage(qubits) =>
      val bitString = createRandomBitString(qubits.length, rng)

      val bitFilter = (qubits zip bitString).map{ case (qubit, bit) =>
        val basis = if(bit == 0) Hadamard else Standard
        qubit.observe(basis)(rng) match {
          case Zero | Plus  => 0
          case One  | Minus => 1
          case _ => throw new IllegalArgumentException("Unknown basis: "+basis)
        }
      }

      addKeyBits(extractKey(bitString, bitFilter))
      sender() ! BitFilterMessage(bitFilter)
  }
}

object Bob extends FixedBasesBobFactory[Bob]{

  override protected def newBob(keyLength: Int, rng: Generator) =
    new Bob(keyLength, rng)
}