package org.waman.worldbrain.system.single.toy

import org.waman.worldbrain.system

sealed abstract class StateBasis(states: Seq[StateVector])
  extends system.StateBasis[StateVector](states)

object StateBasis{
  import StateVector._

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
}