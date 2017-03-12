package org.waman.worldbrain.system

import spire.algebra.Trig
import spire.math._
import spire.implicits._

class NStateSpace2[A: Fractional: Trig]{

  private val v1 = Complex.one[A]
  private val v0 = Complex.zero[A]

  // Standard basis
  /** |0> = |00> */
  val zero: NStateVector[A] = NStateVector.basisVector(2, 0)

  /** |1> = |01> */
  val one: NStateVector[A] = NStateVector.basisVector(2, 1)

  /** |2> = |10> */
  val two: NStateVector[A] = NStateVector.basisVector(2, 2)

  /** |3> = |11> */
  val three: NStateVector[A] = NStateVector.basisVector(2, 3)

  // Bell basis
  /** N(|00> + |11>) */
  val phiPlus: NStateVector[A] = NStateVector(2)(v1, v0, v0, v1)

  /** N(|00> - |11>) */
  val phiMinus: NStateVector[A] = NStateVector(2)(v1, v0, v0, -v1)

  /** N(|01> + |10>) */
  val psiPlus: NStateVector[A] = NStateVector(2)(v0, v1, v1, v0)

  /** N(|01> - |10>) */
  val psiMinus: NStateVector[A] = NStateVector(2)(v0, v1, -v1, v0)

  val standard: NStateBasis[A] = NStateBasis(zero, one, two, three)
  val bell: NStateBasis[A] = NStateBasis(phiPlus, phiMinus, psiPlus, psiMinus)
}

object NStateSpace2 {
  implicit val floatStateSpace      = new NStateSpace2[Float]
  implicit val doubleStateSpace     = new NStateSpace2[Double]
  implicit val bigDecimalStateSpace = new NStateSpace2[BigDecimal]
  implicit val realStateSpace       = new NStateSpace2[Real]
}