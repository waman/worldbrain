package org.waman.worldbrain.single

import org.waman.worldbrain.single.BasisVector._

case class StateBasis(states: Seq[BasisVector])

object StateBasis{
  def apply(v0: BasisVector, v1: BasisVector): StateBasis = apply(Seq(v0, v1))

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
  object Imaginary extends StateBasis(Seq(PlusImaginary, MinusImaginary))
}