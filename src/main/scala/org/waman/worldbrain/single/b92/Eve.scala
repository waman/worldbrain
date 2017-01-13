package org.waman.worldbrain.single.b92

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.StateBasis
import org.waman.worldbrain.single.BasisVector._
import spire.random.Generator

class Eve(alice: ActorRef, bob: ActorRef, protected val bitLength: Int)
         (implicit rng: Generator)
    extends worldbrain.Eve(alice, bob){

  private var bitString: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = StateBasis.createRandomBases(rng, qubits.length)

      this.bitString = (qubits zip bases).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }.map{
        case Zero | Minus => 0
        case One  | Plus  => 1
      }

      this.bob ! m

    case m: BitFilterMessage =>
      addKeyBits(extractKey(this.bitString, m.filter))
      this.alice ! m
  }
}