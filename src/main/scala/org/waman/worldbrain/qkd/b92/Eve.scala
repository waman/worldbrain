package org.waman.worldbrain.qkd.b92

import akka.actor.ActorRef
import org.waman.worldbrain.qkd
import org.waman.worldbrain.qkd.EveFactory
import org.waman.worldbrain.system.single.StateBasis
import spire.math._
import spire.random.Generator

class Eve[A: Fractional] private (alice: ActorRef,
                                  bob: ActorRef,
                                  val keyLength: Int,
                                  protected val bases: Seq[StateBasis[A]],
                                  rng: Generator)
    extends qkd.Eve(alice, bob) with StateAlias[A]{

  private var bitString: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage[A] =>
      val qubits = m.qubits
      val basisSeq = createRandomBases(qubits.length)(rng)

      this.bitString = (qubits zip basisSeq).map{
        case (qubit, basis) => qubit.observe(basis)(rng)
      }.map{
        case s if s == zero | s == minus => 0
        case s if s == one  | s == plus  => 1
      }

      this.bob ! m

    case m: BitFilterMessage =>
      addKeyBits(extractKey(this.bitString, m.filter))
      this.alice ! m
  }
}

object Eve extends EveFactory{

  override protected def newEve[A](alice: ActorRef, bob: ActorRef,
                                   keyLength: Int, bases: Seq[StateBasis[A]],
                                   rng: Generator, a: Fractional[A]): Eve[A] =
    new Eve(alice, bob, keyLength, bases, rng)(a)
}