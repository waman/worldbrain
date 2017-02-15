package org.waman.worldbrain.qkd.bb84

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import spire.math.Fractional
import spire.random.Generator
import org.waman.worldbrain.qkd._
import org.waman.worldbrain.system.single.{StateBasis, BasisVector}
import spire.algebra.Trig

class Ever[A] private (alice: ActorRef, bob: ActorRef, val keyLength: Int, rng: Generator)
                      (implicit a: Fractional[A], trig: Trig[A])
    extends qkd.Eve(alice, bob){

  private var bits: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage[A] =>
      val bases = createRandomBases(m.qubits.length)

      this.bits = (m.qubits zip bases).map{
        case (qubit, basis) =>
          val state = qubit.observe(basis)(rng)
          if(state == basis.states.head) 0 else 1
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      addKeyBits(applyFilter(this.bits, m.filter))
      this.alice ! m
  }

  private def createRandomBases(n: Int): Seq[StateBasis[A]] = {
    Seq.fill(n)(BasisVector.newRandomVectorInReal(rng)(a, trig))
      .map(StateBasis(_)(a))
  }
}

object Ever {

  def apply[A](alice: ActorRef, bob: ActorRef, keyLength: Int, rng: Generator = Generator.rng)
              (implicit a: Fractional[A], trig: Trig[A]): Ever[A] = {
    new Ever(alice, bob, keyLength, rng)(a, trig)
  }
}