package org.waman.worldbrain.system.single

import spire.algebra.Trig
import spire.implicits._
import spire.math._
import org.waman.worldbrain.system.Tolerance

class StateSpace[F](implicit f: Fractional[F], trig: Trig[F], tolerance: Tolerance[F]){

  private val sqrt2inv = 1/sqrt(f.fromInt(2))

  val zero : StateVector[F] = StateVector(1, 0)
  val one  : StateVector[F] = StateVector(0, 1)
  val plus : StateVector[F] = StateVector(sqrt2inv, sqrt2inv)
  val minus: StateVector[F] = StateVector(sqrt2inv, -sqrt2inv)
  val iPlus : StateVector[F] = StateVector(sqrt2inv, Complex.i[F]*sqrt2inv)
  val iMinus: StateVector[F] = StateVector(sqrt2inv, -Complex.i[F]*sqrt2inv)

  val standard: StateBasis[F] = StateBasis(zero, one)
  val hadamard: StateBasis[F] = StateBasis(plus, minus)
  val imaginary: StateBasis[F] = StateBasis(iPlus, iMinus)
}

object StateSpace{
  implicit val floatStateSpace      = new StateSpace[Float]
  implicit val doubleStateSpace     = new StateSpace[Double]
  implicit val bigDecimalStateSpace = new StateSpace[BigDecimal]
  implicit val realStateSpace       = new StateSpace[Real]
}