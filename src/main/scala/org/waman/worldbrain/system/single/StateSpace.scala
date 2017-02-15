package org.waman.worldbrain.system.single

import spire.algebra.{NRoot, Trig}
import spire.math._
import spire.implicits._

class StateSpace[A](implicit a: Fractional[A], nroot: NRoot[A], trig: Trig[A]){

  protected def sqrt2inv: A = a.one / sqrt(a.fromInt(2))

  val zero : BasisVector[A] = BasisVector(a.one, a.zero)
  val one  : BasisVector[A] = BasisVector(a.zero, a.one)
  val plus : BasisVector[A] = BasisVector(sqrt2inv, sqrt2inv)
  val minus: BasisVector[A] = BasisVector(sqrt2inv, -sqrt2inv)
  val iPlus : BasisVector[A] = BasisVector(sqrt2inv, Complex.i[A] * sqrt2inv)
  val iMinus: BasisVector[A] = BasisVector(sqrt2inv, -Complex.i[A] * sqrt2inv)

  val standard: StateBasis[A] = StateBasis(zero, one)
  val hadamard: StateBasis[A] = StateBasis(plus, minus)
  val imaginary: StateBasis[A] = StateBasis(iPlus, iMinus)
}

object StateSpace{
  implicit val FloatStateSpace      = new StateSpace[Float]
  implicit val DoubleStateSpace     = new StateSpace[Double]
  implicit val BigDecimalStateSpace = new StateSpace[BigDecimal]
  implicit val RealStateSpace       = new StateSpace[Real]
}