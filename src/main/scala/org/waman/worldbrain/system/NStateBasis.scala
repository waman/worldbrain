package org.waman.worldbrain.system

import spire.algebra.Trig
import spire.math.Fractional

class NStateBasis[A: Fractional: Trig](val states: Seq[NStateVector[A]]){

  def contains(state: NStateVector[A]): Boolean = this.states.contains(state)
}

object NStateBasis{

  def apply[A: Fractional: Trig](states: NStateVector[A]*): NStateBasis[A] =
    new NStateBasis(Seq(states:_*))
}