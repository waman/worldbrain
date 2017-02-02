package org.waman.worldbrain.system.single

import org.waman.worldbrain.system.single.StateVector._

case class StateBasis(states: Seq[StateVector])

object StateBasis{
  def apply(v0: StateVector, v1: StateVector): StateBasis = apply(Seq(v0, v1))

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
  object Imaginary extends StateBasis(Seq(PlusImaginary, MinusImaginary))
}