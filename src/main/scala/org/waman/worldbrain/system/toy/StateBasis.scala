package org.waman.worldbrain.system.toy

sealed abstract class StateBasis(val states: Seq[StateVector]){

  def contains(state: StateVector): Boolean = this.states.contains(state)
}

object StateBasis{
  import StateVector._

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
}