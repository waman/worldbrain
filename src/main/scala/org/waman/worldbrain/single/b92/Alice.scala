package org.waman.worldbrain.single.b92

import org.waman.worldbrain
import org.waman.worldbrain.single.BasisVector._
import org.waman.worldbrain.single.Protocol.EstablishKey
import org.waman.worldbrain.single.Qubit
import spire.random.Generator

class Alice(implicit rng: Generator)
  extends worldbrain.Alice[Key]{

  private var bitString: BitString = _

  override val establishKeyBehavior: Receive = {
    case EstablishKey(bob, n) =>
      this.bitString = createRandomBitString(n, rng)

      val qubits = this.bitString.map{
        case 0 => Zero
        case 1 => Plus
      }.map(new Qubit(_))

      bob ! QubitMessage(qubits)

    case BitFilterMessage(filter) =>
      setKey(extractKey(this.bitString, filter))
  }
}