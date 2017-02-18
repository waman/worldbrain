package org.waman.worldbrain.system.single

import org.waman.worldbrain.system.Tolerance
import spire.algebra._
import spire.implicits._
import spire.math._
import spire.random.Generator

/** This state vector is normalized and
  *  the first coefficient of this (a if a != 0 or b otherwise) is real and positive. */
abstract class StateVector[A: Fractional: Trig]
    extends GeneralStateVector[A]{
  
  def aReal: A

  override def normalized = this
  
  override def probability(rhs: GeneralStateVector[A]) = probability(rhs.normalized)
  def probability(rhs: StateVector[A]): A
}

object StateVector {

  //********** Implicit algebra objects **********
  implicit val DoubleStateSpace = new StateSpace[Double]
  implicit val RealStateSpace = new StateSpace[Real]

  //********** Implementation classes **********
  private[single] class StateVectorImpl[A: Fractional : Trig]
  (val aReal: A, val b: Complex[A])
    extends StateVector[A] {
    lhs =>

    require(aReal >= 0, s"The first coefficient must be zero or positive: $aReal")

    override def a = Complex(aReal)

    override def norm: A = 1

    override def probability(rhs: StateVector[A]): A = {
      val amplitude = (lhs * rhs).abs
      amplitude * amplitude
    }

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
      new StateVectorImpl(bAbs, Complex(-aReal) * phase)
      // (b*, -a) == (|b|e^(-i phi), -a) == (|b|, -a e^(i phi)
    }
  }

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
      new StateVectorImpl(1, b)

    } else if (a.isZero) {
      new StateVectorImpl(a, Complex.one)

    } else if (a > 0) {
      val norm = sqrt(a * a + (b.conjugate * b).real)
      new StateVectorImpl(a / norm, b / norm)

    }else{
      apply(-a, -b)
    }

  def apply[A: Fractional: Trig](a: A, b: A): StateVector[A] =
    apply(a, Complex(b))


  //********** Other Factories **********
  def ofBlochSphere[A: Fractional: Trig](theta: A): StateVector[A] = {
    val thetaBy2 = theta/2.0
    new StateVectorImpl(cos(thetaBy2), Complex(sin(thetaBy2)))
  }

  def ofBlochSphere[A: Fractional: Trig](theta: A, phi: A)
                                        (implicit tlr: Tolerance[A]): StateVector[A] = {
    val thetaBy2 = theta/2.0
    new StateVectorImpl(cos(thetaBy2), exp(Complex.i[A] * phi) * sin(thetaBy2))
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
    new StateVectorImpl(cosThetaBy2, exp(Complex.i[A] * phi) * sinThetaBy2)
  }
}