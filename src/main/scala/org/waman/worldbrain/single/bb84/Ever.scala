package org.waman.worldbrain.single.bb84

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.{BasisVector, StateBasis}
import spire.random.Generator

class Ever private(alice: ActorRef, bob: ActorRef, basisFactory: BasisFactory, rng: Generator)
    extends worldbrain.Eve[Seq[Int]](alice, bob){

  private var bits: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = basisFactory.createBases(qubits.length)

      this.bits = qubits.zip(bases).map{
        case (qubit, basis) =>
          val state = qubit.observe(basis)(rng)
          if(state == basis.states.head) 0 else 1
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      setKey(applyFilter(this.bits, m.filter))
      this.alice ! m
  }
}

object Ever{

  def apply(alice: ActorRef, bob: ActorRef)(implicit rng: Generator): Ever =
    new Ever(alice, bob, new RandomInRealBasisFactory(rng), rng)

  def apply(alice: ActorRef, bob: ActorRef, bf: BasisFactory)(implicit rng: Generator): Ever =
    new Ever(alice, bob, bf, rng)
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