package org.waman.worldbrain.single.bb84

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.bb84.BB84._
import org.waman.worldbrain.single.{BasisVector, StateBasis}
import spire.random.Generator

class Ever private(alice: ActorRef, bob: ActorRef, basisFactory: BasisFactory)
    extends worldbrain.Eve[Seq[Int]](alice, bob){

  private var bits: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = basisFactory.createBases(qubits.length)

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
}

object Ever{

  def apply(alice: ActorRef, bob: ActorRef)(implicit rng: Generator): Ever =
    new Ever(alice, bob, new RandomInRealBasisFactory(rng))

  def apply(alice: ActorRef, bob: ActorRef, bf: BasisFactory): Ever =
    new Ever(alice, bob, bf)
}

trait BasisFactory{
  def createBases(n: Int): Seq[StateBasis] =
    (0 until n).map(_ => createBasis)

  def createBasis: StateBasis
}

class FixedBasisFactory(theta: Double, phi: Double = 0.0) extends BasisFactory{
  val basis: StateBasis = BasisVector.getBasis(BasisVector.ofBlochSphere(theta, phi))

  override def createBasis = basis
}

class RandomInRealBasisFactory(rng: Generator) extends BasisFactory{
  override def createBasis =
    BasisVector.getBasis(BasisVector.newRandomVectorInReal(rng))
}

class RandomBasisFactory(rng: Generator) extends BasisFactory{
  override def createBasis =
      BasisVector.getBasis(BasisVector.newRandomVector(rng))
}