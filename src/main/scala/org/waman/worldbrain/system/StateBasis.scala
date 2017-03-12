package org.waman.worldbrain.system

import spire.math.Fractional

case class StateBasis[A: Fractional](states: Seq[StateVector[A]]){

  require(states.length == 2,
    "Single qubit basis must have only two state vectors: " + states.length)

  def contains(state: StateVector[A]): Boolean = states.contains(state)
  def indexOf(state: StateVector[A]): Int = states.indexOf(state)

  def first : StateVector[A] = states.head
  def second: StateVector[A] = states(1)
}

object StateBasis{

  def apply[A: Fractional](v0: StateVector[A], v1: StateVector[A]): StateBasis[A] = {
    require(v0 isOrthogonalTo v1)
    new StateBasis(Seq(v0, v1))
  }

  def apply[F: Fractional](state: StateVector[F]): StateBasis[F] =
    new StateBasis(Seq(state, state.getPerpendicular))
}