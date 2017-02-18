package org.waman.worldbrain.system.single

import org.waman.worldbrain.system
import spire.math.Fractional

class StateBasis[A: Fractional](states: Seq[StateVector[A]])
    extends system.StateBasis(states){
}

object StateBasis{
  def apply[A: Fractional](v0: StateVector[A], v1: StateVector[A]): StateBasis[A] = {
    require(v0 * v1 == implicitly[Fractional[A]].zero)
    new StateBasis(Seq(v0, v1))
  }

  def apply[F: Fractional](state: StateVector[F]): StateBasis[F] =
    new StateBasis(Seq(state, state.getPerpendicular))
}