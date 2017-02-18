package org.waman.worldbrain.system.single

import spire.algebra.Trig
import spire.implicits._
import spire.math._
import org.waman.worldbrain.system.Tolerance

class StateSpace[A: Fractional: Trig](implicit tolerance: Tolerance[A]){

  val zero : StateVector[A] = StateVector(1, 0)
  val one  : StateVector[A] = StateVector(0, 1)
  val plus : StateVector[A] = StateVector(1, 1)
  val minus: StateVector[A] = StateVector(1, -1)
  val iPlus : StateVector[A] = StateVector[A](1, Complex.i[A])
  val iMinus: StateVector[A] = StateVector[A](1, -Complex.i[A])

  val standard: StateBasis[A] = StateBasis(zero, one)
  val hadamard: StateBasis[A] = StateBasis(plus, minus)
  val imaginary: StateBasis[A] = StateBasis(iPlus, iMinus)
}

object StateSpace{
  implicit val floatStateSpace      = new StateSpace[Float]
  implicit val doubleStateSpace     = new StateSpace[Double]
  implicit val bigDecimalStateSpace = new StateSpace[BigDecimal]
  implicit val realStateSpace       = new StateSpace[Real]
}