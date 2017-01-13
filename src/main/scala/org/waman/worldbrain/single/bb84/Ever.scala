package org.waman.worldbrain.single.bb84

import akka.actor.ActorRef
import org.waman.worldbrain
import org.waman.worldbrain.single.{BasisVector, StateBasis}
import org.waman.worldbrain.KeyContainer._
import spire.random.Generator

class Ever private(alice: ActorRef, bob: ActorRef, protected val bitLength: Int,
                   basisFactory: BasisFactory, rng: Generator)
    extends worldbrain.Eve(alice, bob){

  require(bitLength > 0)

  private var bits: Seq[Int] = _

  override val eavesdropBehavior: Receive = {
    case m: QubitMessage =>
      val qubits = m.qubits
      val bases = basisFactory.createBases(qubits.length)

      this.bits = (qubits zip bases).map{
        case (qubit, basis) =>
          val state = qubit.observe(basis)(rng)
          if(state == basis.states.head) 0 else 1
      }

      this.bob ! m

    case m: BasisFilterMessage =>
      addKeyBits(applyFilter(this.bits, m.filter))
      this.alice ! m
  }
}

object Ever{

  def apply(alice: ActorRef, bob: ActorRef, bitLength: Int)(implicit rng: Generator): Ever =
    new Ever(alice, bob, bitLength, new RandomInRealBasisFactory(rng), rng)

  def apply(alice: ActorRef, bob: ActorRef, bitLength: Int, bf: BasisFactory)
           (implicit rng: Generator): Ever =
    new Ever(alice, bob, bitLength, bf, rng)
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