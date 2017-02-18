package org.waman.worldbrain.system.single

import org.waman.worldbrain.system
import spire.math.Fractional

class StateBasis[F: Fractional](states: Seq[StateVector[F]])
    extends system.StateBasis(states){
}

object StateBasis{
  def apply[F](v0: StateVector[F], v1: StateVector[F])(implicit f: Fractional[F]): StateBasis[F] = {
    require(v0 * v1 == f.zero)
    new StateBasis(Seq(v0, v1))
  }

  def apply[F: Fractional](state: StateVector[F]): StateBasis[F] =
    new StateBasis(Seq(state, state.getPerpendicular))
}