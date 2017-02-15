package org.waman.worldbrain.system.single

import org.waman.worldbrain.system
import spire.math.Fractional

class StateBasis[A: Fractional](states: Seq[BasisVector[A]])
    extends system.StateBasis(states){
}

object StateBasis{
  def apply[A: Fractional](v0: BasisVector[A], v1: BasisVector[A]): StateBasis[A] =
    new StateBasis(Seq(v0, v1))

  def apply[A: Fractional](state: BasisVector[A]): StateBasis[A] =
    StateBasis(state, state.getPerpendicular)
}