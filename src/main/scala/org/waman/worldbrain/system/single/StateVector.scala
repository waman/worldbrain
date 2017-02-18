package org.waman.worldbrain.system.single

import spire.math._
import spire.algebra._
import spire.implicits._
import org.waman.worldbrain.system
import org.waman.worldbrain.system.Tolerance
import spire.random.Generator

/** This state vector is normalized and
  *  the first coefficient of this (a if a != 0 or b otherwise) is real and positive. */
abstract class StateVector[F](implicit f: Fractional[F], trig: Trig[F])
    extends GeneralStateVector[F]{
  
  def aReal: F

  override def normalized = this
  
  override def probability(rhs: GeneralStateVector[F]) = probability(rhs.normalized)
  def probability(rhs: StateVector[F]): F
}

object StateVector {

  //********** Implicit algebra objects **********
  implicit val DoubleStateSpace = new StateSpace[Double]
  implicit val RealStateSpace = new StateSpace[Real]

  //********** Implementation classes **********
  private[single] class StateVectorImpl[F](val aReal: F, val b: Complex[F])
                                          (implicit f: Fractional[F], trig: Trig[F])
      extends StateVector[F]{ lhs =>

    require(aReal >= 0, s"The first coefficient must be zero or positive: $aReal")

    override def a = Complex(aReal)

    override def norm: F = f.one

    override def probability(rhs: StateVector[F]): F = {
      val amplitude = (lhs * rhs).abs
      amplitude * amplitude
    }

    /** (theta, phi) */
    override def toAnglesOfBlochSphere: (F, F) = {
      val phi_ = b.arg
      val phi = if(phi_ >= 0) phi_ else phi_ + 2 * pi
      (2*acos(aReal), phi)
    }

    override def toPointOnBlochSphere: (F, F, F) = {
      val (theta, phi) = toAnglesOfBlochSphere
      val sinTheta: F = sin(theta)
      (sinTheta*cos(phi), sinTheta*sin(phi), cos(theta))
    }

    /** Return null if (0, 0, 1) (infinity) */
    override def toComplex: Complex[F] = toPointOnBlochSphere match {
      case (0, 0, 1) => null
      case (x, y, z) =>
        val denom = 1-z
        Complex(x/denom, y/denom)
    }

    override def getPerpendicular: StateVector[F] = {
      val bAbs = b.abs
      val phase = b/bAbs
      new StateVectorImpl(bAbs, Complex(-aReal)*phase)
      // (b*, -a) == (|b|e^(-i phi), -a) == (|b|, -a e^(i phi)
    }
  }

  //********** apply() factory methods **********
  def apply[F](a: Complex[F], b: Complex[F])
              (implicit f: Fractional[F], trig: Trig[F], tlr: Tolerance[F]): StateVector[F] =
    if(a.isReal) {
      apply(a.real, b)

    }else{
      val aAbs = a.abs
      val phase = a/aAbs
      apply(aAbs, b/phase)
    }

  def apply[F](a: F, b: Complex[F])
              (implicit f: Fractional[F], trig: Trig[F], tlr: Tolerance[F]): StateVector[F] = {
    if(a > 0) {
      val norm2 = a * a + (b.conjugate * b).real
      if (tlr.test(norm2, f.one)) {
        new StateVectorImpl(a, b)
      } else {
        val norm = sqrt(norm2)
        new StateVectorImpl(a / norm, b / norm)
      }

    }else if(a < 0) {
      apply(-a, -b)

    }else{
      new StateVectorImpl(f.zero, Complex.one)
    }
  }

  def apply[F](a: F, b: F)
              (implicit f: Fractional[F], trig: Trig[F], tlr: Tolerance[F]): StateVector[F] =
    apply(a, Complex(b))


  //********** Other Factories **********
  def ofBlochSphere[F](theta: F)
                      (implicit f: Fractional[F], trig: Trig[F]): StateVector[F] = {
    val thetaBy2 = theta/2.0
    new StateVectorImpl(cos(thetaBy2), Complex(sin(thetaBy2)))
  }

  def ofBlochSphere[F](theta: F, phi: F)
                      (implicit f: Fractional[F], trig: Trig[F], tlr: Tolerance[F]): StateVector[F] = {
    val thetaBy2 = theta/2.0
    new StateVectorImpl(cos(thetaBy2), exp(Complex.i[F] * phi) * sin(thetaBy2))
  }

  def newRandomVectorInReal[F](implicit f: Fractional[F], trig: Trig[F], rng: Generator): StateVector[F] =
    newRandomVectorInReal(rng)

  def newRandomVectorInReal[F](rng: Generator)
                              (implicit f: Fractional[F], trig: Trig[F]): StateVector[F] =
    ofBlochSphere(pi[F] * rng.nextDouble)

  def newRandomVector[F](implicit f: Fractional[F], trig: Trig[F], rng: Generator): StateVector[F] =
    newRandomVector[F](rng)

  def newRandomVector[F](rng: Generator)
                        (implicit f: Fractional[F], trig: Trig[F]): StateVector[F] = {
    val cosThetaBy2 = f.fromDouble(rng.nextDouble)
    val sinThetaBy2 = sqrt(1 - cosThetaBy2*cosThetaBy2)
    val phi = 2 * pi[F] * rng.nextDouble
    new StateVectorImpl(cosThetaBy2, exp(Complex.i[F] * phi) * sinThetaBy2)
  }
}