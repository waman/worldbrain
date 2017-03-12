package org.waman.worldbrain.system

import spire.implicits._
import spire.math.Fractional
import spire.random.Generator

abstract class Qubit[A: Fractional]{

  def observe(basis: StateBasis[A])(implicit rng: Generator): StateVector[A]

  def observeInStandardBasis(implicit ss: StateSpace[A], rng: Generator): StateVector[A] =
    observe(ss.standard)(rng)

  def observeInHadamardBasis(implicit ss: StateSpace[A], rng: Generator): StateVector[A] =
    observe(ss.hadamard)(rng)
}

object Qubit{

  private class QubitImpl[A](private var state: StateVector[A])
                            (implicit f: Fractional[A]) extends Qubit[A]{

    override def observe(basis: StateBasis[A])(implicit rng: Generator): StateVector[A] =
      synchronized{
        val v = basis.first
        val p = this.state probability v
        if(rng.nextDouble < p)
          this.state = v
        else
          this.state = basis.second

        this.state
      }
  }


  def apply[A: Fractional](state: StateVector[A]): Qubit[A] =
    new QubitImpl(state)
}