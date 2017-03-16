package org.waman.worldbrain.system

import spire.algebra.Trig
import spire.math._
import spire.implicits._

class NStateSpace2[A: Fractional: Trig]{

  private def cZero = Complex.zero[A]
  private def cOne = Complex.one[A]

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
  val phiPlus: NStateVector[A] = NStateVector(2)(cOne, cZero, cZero, cOne)

  /** N(|00> - |11>) */
  val phiMinus: NStateVector[A] = NStateVector(2)(cOne, cZero, cZero, -cOne)

  /** N(|01> + |10>) */
  val psiPlus: NStateVector[A] = NStateVector(2)(cZero, cOne, cOne, cZero)

  /** N(|01> - |10>) */
  val psiMinus: NStateVector[A] = NStateVector(2)(cZero, cOne, -cOne, cZero)

  val standard: NStateBasis[A] = NStateBasis(zero, one, two, three)
  val bell: NStateBasis[A] = NStateBasis(phiPlus, phiMinus, psiPlus, psiMinus)
}

object NStateSpace2 {
  implicit val floatStateSpace      = new NStateSpace2[Float]
  implicit val doubleStateSpace     = new NStateSpace2[Double]
  implicit val bigDecimalStateSpace = new NStateSpace2[BigDecimal]
  implicit val realStateSpace       = new NStateSpace2[Real]
}