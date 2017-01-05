package org.waman.quantum.single.bb84

import akka.actor.ActorRef
import org.waman.quantum
import org.waman.quantum.single.BasisVector
import org.waman.quantum.single.bb84.BB84._

class Eve(alice: ActorRef, bob: ActorRef)
    extends quantum.Eve[Seq[Int]](alice, bob){

  private var states: Seq[BasisVector] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = createRandomBases(qubits.length)

      this.states = qubits.zip(bases).map{
        case (qubit, basis) => qubit.observe(basis)
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      setKey(extractKey(this.states, m.filter))
      this.alice ! m
  }
}