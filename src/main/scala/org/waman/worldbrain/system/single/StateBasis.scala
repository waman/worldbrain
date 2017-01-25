package org.waman.worldbrain.system.single

import org.waman.worldbrain.system.single.BasisKet._
import spire.random.Generator

case class StateBasis(states: Seq[BasisKet])

object StateBasis{
  def apply(v0: BasisKet, v1: BasisKet): StateBasis = apply(Seq(v0, v1))

  def createRandomBases(rng: Generator, n: Int): Seq[StateBasis] =
    (0 until n).map{ _ =>
      if(rng.nextBoolean) Standard
      else                Hadamard
    }

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
  object Imaginary extends StateBasis(Seq(PlusImaginary, MinusImaginary))
}