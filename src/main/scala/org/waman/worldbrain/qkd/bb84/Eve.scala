package org.waman.worldbrain.qkd.bb84

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.system.{StateBasis, StateSpace, StateVector}
import spire.random.Generator

class Eve[A: Fractional] private (alice: ActorRef, bob: ActorRef,
                                  val keyLength: Int,
                                  val bases: Seq[StateBasis[A]],
                                  rng: Generator)
    extends qkd.Eve(alice, bob) with StateEncoder[A]{

  private var states: Seq[StateVector[A]] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage[A] =>
      val qubits = m.qubits
      val bases = createRandomBases(qubits.length)(rng)

      this.states = (qubits zip bases).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      addKeyBits(extractKey(this.states, m.filter))
      this.alice ! m
  }
}

object Eve{

  def apply[A](alice: ActorRef, bob: ActorRef,
               keyLength: Int,
               bases: Seq[StateBasis[A]] = Nil,
               rng: Generator = Generator.rng)
              (implicit a: Fractional[A], ss: StateSpace[A]): Eve[A] = {
    val b = if(bases.isEmpty) Seq(ss.standard, ss.hadamard) else bases
    new Eve(alice, bob, keyLength, b, rng)
  }
}