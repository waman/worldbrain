package org.waman.worldbrain.single.bb84

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.BasisVector
import spire.random.Generator

class Eve(alice: ActorRef, bob: ActorRef)(implicit rng: Generator)
    extends worldbrain.Eve[Seq[Int]](alice, bob){

  private var states: Seq[BasisVector] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = createRandomBases(rng, qubits.length)

      this.states = qubits.zip(bases).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      setKey(extractKey(this.states, m.filter))
      this.alice ! m
  }
}