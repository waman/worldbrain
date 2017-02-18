package org.waman.worldbrain.qkd.bb84.toy

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.FixedBasesEveFactory
import org.waman.worldbrain.system.single.toy.StateVector
import spire.random.Generator

class Eve private (alice: ActorRef, bob: ActorRef, val keyLength: Int, rng: Generator)
    extends qkd.Eve(alice, bob){

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

object Eve extends FixedBasesEveFactory[Eve]{

  override protected def newEve(alice: ActorRef, bob: ActorRef,
                                keyLength: Int, rng: Generator) =
    new Eve(alice, bob, keyLength, rng)
}