package org.waman.worldbrain

import spire.math.Complex
import spire.implicits._

import scala.math.sqrt

package object single{

  private val sqrt2inv: Complex[Double] = Complex(1.0/sqrt(2))
  private val i: Complex[Double] = Complex.i[Double]

  object Zero extends BasisVector(1, 0){
    override def toString: String = "|0>"
  }

  object One extends BasisVector(0, 1){
    override def toString: String = "|1>"
  }

  object Plus extends BasisVector(sqrt2inv, sqrt2inv){
    override def toString: String = "|+>"
  }

  object Minus extends BasisVector(sqrt2inv, -sqrt2inv){
    override def toString: String = "|->"
  }

  object PlusImaginary extends BasisVector(sqrt2inv, i*sqrt2inv){
    override def toString: String = "|i>"
  }

  object MinusImaginary extends BasisVector(sqrt2inv, -i*sqrt2inv){
    override def toString: String = "|-i>"
  }

  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
  object Imaginary extends StateBasis(Seq(PlusImaginary, MinusImaginary))
}