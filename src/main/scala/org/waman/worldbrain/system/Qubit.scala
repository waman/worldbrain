package org.waman.worldbrain.system

import spire.random.Generator

trait Qubit[V <: StateVector, B <: StateBasis[V]]{

  def observe(basis: B)(implicit rng: Generator): V
}