package org.waman.worldbrain.qkd.b92

import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.BobFactory
import org.waman.worldbrain.system.StateBasis
import spire.math.Fractional
import spire.random.Generator

class Bob[A: Fractional] private (val keyLength: Int,
                                  protected val bases: Seq[StateBasis[A]],
                                  rng: Generator)
    extends qkd.Bob with StateAlias[A]{

  override val establishKeyBehavior: Receive = {
    case m: QubitMessage[A] =>
      val bitString = createRandomBitString(m.qubits.length)(rng)

      val bitFilter = (m.qubits zip bitString).map{ case (qubit, bit) =>
        val basis = if(bit == 0) hadamard else standard
        qubit.observe(basis)(rng) match {
          case s if s == zero || s == plus  => 0
          case s if s == one  || s == minus => 1
        }
      }

      addKeyBits(extractKey(bitString, bitFilter))
      sender() ! BitFilterMessage(bitFilter)
  }
}

object Bob extends BobFactory{

  override protected def newBob[A](keyLength: Int, bases: Seq[StateBasis[A]],
                                   rng: Generator, a: Fractional[A]): Bob[A] =
    new Bob(keyLength, bases, rng)(a)
}