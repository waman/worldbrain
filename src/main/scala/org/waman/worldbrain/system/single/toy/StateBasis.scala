package org.waman.worldbrain.system.single.toy

import org.waman.worldbrain.system

sealed abstract class StateBasis(states: Seq[BasisVector])
  extends system.StateBasis[BasisVector](states)

object StateBasis{
  import BasisVector._

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
}