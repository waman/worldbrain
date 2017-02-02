package org.waman.worldbrain.qkd.bb84

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.system.single.StateVector
import spire.random.Generator

class Eve(alice: ActorRef, bob: ActorRef, val keyLength: Int)
         (implicit rng: Generator)
    extends qkd.Eve(alice, bob){

  require(keyLength > 0)

  private var states: Seq[StateVector] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = createRandomBases(rng, qubits.length)

      this.states = (qubits zip bases).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      addKeyBits(extractKey(this.states, m.filter))
      this.alice ! m
  }
}