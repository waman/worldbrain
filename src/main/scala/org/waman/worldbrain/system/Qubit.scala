package org.waman.worldbrain.system

import spire.random.Generator

trait Qubit[V <: BasisVector, B <: StateBasis[V]]{

  def observe(basis: B)(implicit rng: Generator): V
}

object Qubit{
}