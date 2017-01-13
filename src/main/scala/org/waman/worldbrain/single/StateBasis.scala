package org.waman.worldbrain.single

import org.waman.worldbrain.single.BasisVector._
import spire.random.Generator

case class StateBasis(states: Seq[BasisVector])

object StateBasis{
  def apply(v0: BasisVector, v1: BasisVector): StateBasis = apply(Seq(v0, v1))

  def createRandomBases(rng: Generator, n: Int): Seq[StateBasis] =
    (0 until n).map{ i =>
      if(rng.nextBoolean) Standard
      else                Hadamard
    }

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
  object Imaginary extends StateBasis(Seq(PlusImaginary, MinusImaginary))
}