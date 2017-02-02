package org.waman.worldbrain.qkd.b92

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.system.single.StateVector._
import spire.random.Generator

class Eve(alice: ActorRef, bob: ActorRef, val keyLength: Int)
         (implicit rng: Generator)
    extends qkd.Eve(alice, bob){

  private var bitString: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = createRandomBases(rng, qubits.length)

      this.bitString = (qubits zip bases).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }.map{
        case Zero | Minus => 0
        case One  | Plus  => 1
        case s => throw new IllegalArgumentException("Unknown state: "+s)
      }

      this.bob ! m

    case m: BitFilterMessage =>
      addKeyBits(extractKey(this.bitString, m.filter))
      this.alice ! m
  }
}