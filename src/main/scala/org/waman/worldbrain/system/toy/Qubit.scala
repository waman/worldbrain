package org.waman.worldbrain.system.toy

import spire.random.Generator

trait Qubit{

  def observe(basis: StateBasis)(implicit rng: Generator): StateVector

  def observeInStandardBasis(implicit rng: Generator): StateVector =
    observe(StateBasis.Standard)(rng)

  def observeInHadamardBasis(implicit rng: Generator): StateVector =
    observe(StateBasis.Hadamard)(rng)
}

object Qubit{

  def apply(state: StateVector): Qubit = new QubitImpl(state)

  private class QubitImpl(private var state: StateVector) extends Qubit {

    override def observe(basis: StateBasis)(implicit rng: Generator): StateVector =
      synchronized {
        if (basis.contains(this.state))
          this.state
        else {
          if (rng.nextBoolean) basis.states.head
          else basis.states(1)
        }
      }
  }
}