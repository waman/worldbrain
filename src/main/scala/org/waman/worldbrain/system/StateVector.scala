package org.waman.worldbrain.system

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.random.Generator

/** This state vector is normalized and
  *  the first coefficient of this (a if a != 0 or b otherwise) is real and positive. */
class StateVector[A: Fractional : Trig] private (val aReal: A, val b: Complex[A])
    extends GeneralStateVector[A]{ lhs =>

  require(aReal >= 0, s"The first coefficient must be zero or positive: $aReal")

  def a = Complex(aReal)
  def norm: A = 1

  def isOrthogonalTo(rhs: StateVector[A]): Boolean =
    lhs * rhs == implicitly[Field[Complex[A]]].zero

  override def probability(rhs: GeneralStateVector[A]) = probability(rhs.normalized)

  def probability(rhs: StateVector[A]): A = {
    val amplitude = (lhs * rhs).abs
    amplitude * amplitude
  }

  override def normalized = this

  /** (theta, phi) */
  override def toAnglesOfBlochSphere: (A, A) = {
    val phi_ = b.arg
    val phi = if (phi_ >= 0) phi_ else phi_ + 2 * pi
    (2 * acos(aReal), phi)
  }

  override def toPointOnBlochSphere: (A, A, A) = {
    val (theta, phi) = toAnglesOfBlochSphere
    val sinTheta: A = sin(theta)
    (sinTheta * cos(phi), sinTheta * sin(phi), cos(theta))
  }

  /** Return null if (0, 0, 1) (infinity) */
  override def toComplex: Complex[A] = toPointOnBlochSphere match {
    case (0, 0, 1) => null
    case (x, y, z) =>
      val denom = 1 - z
      Complex(x / denom, y / denom)
  }

  override def getPerpendicular: StateVector[A] = {
    val bAbs = b.abs
    val phase = b / bAbs
    new StateVector(bAbs, Complex(-aReal) * phase)
    // (b*, -a) == (|b|e^(-i phi), -a) == (|b|, -a e^(i phi)
  }
}

object StateVector {

  //********** Implicit algebra objects **********
  implicit val DoubleStateSpace = new StateSpace[Double]
  implicit val RealStateSpace = new StateSpace[Real]

  //********** apply() factory methods **********
  def apply[A: Fractional : Trig](a: Complex[A], b: Complex[A]): StateVector[A] =
    if (a.isReal) {
      apply(a.real, b)

    } else {
      val aAbs = a.abs
      val phase = a / aAbs
      apply(aAbs, b / phase)
    }

  def apply[A: Fractional : Trig](a: A, b: Complex[A]): StateVector[A] =
    if (b.isZero) {
      new StateVector(1, b)

    } else if (a.isZero) {
      new StateVector(a, Complex.one)

    } else if (a > 0) {
      val norm = sqrt(a * a + (b.conjugate * b).real)
      new StateVector(a / norm, b / norm)

    }else{
      apply(-a, -b)
    }

  def apply[A: Fractional: Trig](a: A, b: A): StateVector[A] =
    apply(a, Complex(b))


  //********** Other Factories **********
  def ofBlochSphere[A: Fractional: Trig](theta: A): StateVector[A] = {
    val thetaBy2 = theta/2.0
    new StateVector(cos(thetaBy2), Complex(sin(thetaBy2)))
  }

  def ofBlochSphere[A: Fractional: Trig](theta: A, phi: A)
                                        (implicit tlr: Tolerance[A]): StateVector[A] = {
    val thetaBy2 = theta/2.0
    new StateVector(cos(thetaBy2), exp(Complex.i[A] * phi) * sin(thetaBy2))
  }

  def newRandomVectorInReal[A: Fractional: Trig](implicit rng: Generator): StateVector[A] =
    newRandomVectorInReal(rng)

  def newRandomVectorInReal[A: Fractional: Trig](rng: Generator): StateVector[A] =
    ofBlochSphere(pi[A] * rng.nextDouble)

  def newRandomVector[A: Fractional: Trig](implicit rng: Generator): StateVector[A] =
    newRandomVector[A](rng)

  def newRandomVector[A: Fractional: Trig](rng: Generator): StateVector[A] = {
    val cosThetaBy2 = implicitly[Fractional[A]].fromDouble(rng.nextDouble)
    val sinThetaBy2 = sqrt(1 - cosThetaBy2*cosThetaBy2)
    val phi = 2 * pi[A] * rng.nextDouble
    new StateVector(cosThetaBy2, exp(Complex.i[A] * phi) * sinThetaBy2)
  }
}