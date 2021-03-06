package org.waman.worldbrain.qkd.b92.toy

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.FixedBasesEveFactory
import org.waman.worldbrain.system.toy.StateVector._
import spire.random.Generator

class Eve private (alice: ActorRef, bob: ActorRef, val keyLength: Int, rng: Generator)
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
      }

      this.bob ! m

    case m: BitFilterMessage =>
      addKeyBits(extractKey(this.bitString, m.filter))
      this.alice ! m
  }
}

object Eve extends FixedBasesEveFactory[Eve]{

  override protected def newEve(alice: ActorRef, bob: ActorRef, keyLength: Int, rng: Generator) =
    new Eve(alice, bob, keyLength, rng)
}