package org.waman.worldbrain.single

case class StateBasis(states: Seq[BasisVector])

object StateBasis{
  def apply(v0: BasisVector, v1: BasisVector): StateBasis = apply(Seq(v0, v1))
}