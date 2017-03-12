package org.waman.worldbrain.system.toy

import spire.random.Generator
import StateBasis._

trait Qubit2{

  def observe(basis: StateBasis2)(implicit rng: Generator): StateVector2

  def observe(i: Int, basis: StateBasis)(implicit rng: Generator): StateVector =
    basis match {
      case Standard => observeInStandardBasis(i)
      case Hadamard => observeInHadamardBasis(i)
    }

  def observeInStandardBasis(i: Int)(implicit rng: Generator): StateVector
  def observeInHadamardBasis(i: Int)(implicit rng: Generator): StateVector

  def apply(i: Int) = new Qubit{

    override def observe(basis: StateBasis)(implicit rng: Generator) =
      Qubit2.this.observe(i, basis)(rng)
  }
}

object Qubit2{

  def apply(state: StateVector2): Qubit2 = new Qubits2Impl(state)

  private class Qubits2Impl(private var state: StateVector2) extends Qubit2{

    override def observe(basis: StateBasis2)(implicit rng: Generator): StateVector2 = ???

    override def observeInStandardBasis(i: Int)(implicit rng: Generator): StateVector = ???

    override def observeInHadamardBasis(i: Int)(implicit rng: Generator): StateVector = ???
  }
}