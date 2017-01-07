package org.waman.worldbrain.single.bb84

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.bb84.BB84._
import org.waman.worldbrain.single.{BasisVector, StateBasis}
import spire.random.Generator

class Ever(alice: ActorRef, bob: ActorRef)(implicit rng: Generator)
    extends worldbrain.Eve[Seq[Int]](alice, bob){

  private var bits: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = createRandomBases(qubits.length)

      this.bits = qubits.zip(bases).map{
        case (qubit, basis) =>
          val state = qubit.observe(basis)
          if(state == basis.states.head) 0 else 1
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      setKey(this.bits.zip(m.filter).filter(_._2).map(_._1))
      this.alice ! m
  }

  private val obsBasis: StateBasis = createBasis(Math.PI * 2.0 / 3.0)

  private def createBasis(theta: Double): StateBasis = {
    val v = BasisVector.ofBlochSphere(theta)
    StateBasis(Seq(v, v.getPerpendicular))
  }

  private def createRandomBases(n: Int): Seq[StateBasis] = {
    (0 until n).map { _ =>
      this.obsBasis
//      BasisVector.getBasis(BasisVector.newRandomVectorInReal)
//      BasisVector.getBasis(BasisVector.newRandomVector)
    }
  }
}