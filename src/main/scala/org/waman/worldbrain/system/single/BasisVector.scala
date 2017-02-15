package org.waman.worldbrain.system.single

import spire.math._
import spire.implicits._
import org.waman.worldbrain.system
import org.waman.worldbrain.system.Tolerance
import spire.algebra.{NRoot, Trig}
import spire.random.Generator

class BasisVector[A] private(val aReal: A, val b: Complex[A])
                            (implicit algebra: Fractional[A], trig: Trig[A])
    extends system.BasisVector{

  def a: Complex[A] = Complex(aReal)
  require(aReal >= 0, s"The first coefficient must be zero or positive: $aReal")
//  require(normTolerance.test(
//    aReal*aReal + ((~b)*b).abs,
//    implicitly[Fractional[A]].fromInt(1), "The norm must be 1")

  def *(that: BasisVector[A]): Complex[A] =
    Complex(aReal * that.aReal) + b.conjugate * that.b

  def probability(that: BasisVector[A]): A = {
    val amplitude = (this * that).abs
    amplitude * amplitude
  }

  /** (theta, phi) */
  def toAnglesOfBlochSphere: (A, A) = {
    val phi_ = b.arg
    val phi = if(phi_ >= algebra.zero) phi_ else phi_ + 2 * pi
    (2*acos(aReal), phi)
  }

  def toPointOnBlochSphere: (A, A, A) = {
    val (theta, phi) = toAnglesOfBlochSphere
    val sinTheta: A = sin(theta)
    (sinTheta*cos(phi), sinTheta*sin(phi), cos(theta))
  }

  /** Return null if (0, 0, 1) (infinity) */
  def toComplex: Complex[A] = toPointOnBlochSphere match {
    case (0, 0, 1) => null
    case (x, y, z) =>
      val denom = 1-z
      Complex(x/denom, y/denom)
  }

  def toTuple: (A, Complex[A]) = (aReal, b)
  def toComplexTuple: (Complex[A], Complex[A]) = (Complex(aReal), b)

  def getPerpendicular: BasisVector[A] = {
    val bAbs = b.abs
    val phase = b/bAbs
    new BasisVector(bAbs, Complex(-aReal)*phase)
    // (b*, -a) == (|b|e^(-i phi), -a) == (|b|, -a e^(i phi)
  }

  //***** equivalency *****
  def hasTheSameCoefficientsAs(that: BasisVector[A])(implicit t: Tolerance[A]): Boolean =
    t.test(aReal, that.aReal) && t.test(b, that.b)

  def isTheSameStateAs(that: BasisVector[A])(implicit t: Tolerance[A]): Boolean =
    t.test((this * that).abs, implicitly[Fractional[A]].fromInt(1))

  override def toString: String =
    s"($aReal)|0> + ($b)|1>"
}

object BasisVector {

  implicit val DoubleStateSpace = new StateSpace[Double]
  implicit val RealStateSpace = new StateSpace[Real]

  private[BasisVector] def normTolerance[A](implicit ev: Fractional[A]) =
    Tolerance(ev.fromDouble(1e-10))

  def normalized[A](a: Complex[A], b: Complex[A])
                   (implicit algebra: Fractional[A], trig: Trig[A]): BasisVector[A] = {
    val norm = sqrt(a.conjugate*a + b.conjugate*b)
    apply(a/norm, b/norm)
  }

  def apply[A](a: A, b: A)
              (implicit algebra: Fractional[A], trig: Trig[A]): BasisVector[A] =
    apply(a, Complex(b))

  def apply[A](a: A, b: Complex[A])
              (implicit algebra: Fractional[A], trig: Trig[A]): BasisVector[A] =
    a match {
      case _ if a.isZero => new BasisVector(a, Complex(b.abs))
      case _ if a > 0    => new BasisVector(a, b)
      case _             => new BasisVector(-a, -b)
    }

  def apply[A](a: Complex[A], b: Complex[A])
              (implicit algebra: Fractional[A], trig: Trig[A]): BasisVector[A] =
    a match {
      case _ if a.isZero =>
        new BasisVector(a.real, Complex(b.abs))
      case _ =>
        val aAbs = a.abs
        val phase = a/aAbs
        new BasisVector(aAbs, b/phase)
    }

  def ofBlochSphere[A](theta: A)(implicit a: Fractional[A], trig: Trig[A]): BasisVector[A] = {
    val thetaBy2 = theta/2.0
    new BasisVector(cos(thetaBy2), Complex(sin(thetaBy2)))
  }

  def ofBlochSphere[A](theta: A, phi: A)(implicit a: Fractional[A], trig: Trig[A]): BasisVector[A] = {
    val thetaBy2 = theta/2.0
    new BasisVector(cos(thetaBy2), exp(Complex.i[A] * phi) * sin(thetaBy2))
  }

  def newRandomVectorInReal[A](implicit a: Fractional[A], trig: Trig[A], rng: Generator): BasisVector[A] =
    newRandomVectorInReal(rng)(a, trig)

  def newRandomVectorInReal[A](rng: Generator)
                              (implicit a: Fractional[A], trig: Trig[A]): BasisVector[A] =
    ofBlochSphere(pi[A] * rng.nextDouble)

  def newRandomVector[A](implicit a: Fractional[A],
                         nroot: NRoot[A],
                         trig: Trig[A], rng: Generator): BasisVector[A] =
    newRandomVector[A](rng)(a, nroot, trig)

  def newRandomVector[A](rng: Generator)
                        (implicit a: Fractional[A], nroot: NRoot[A], trig: Trig[A]): BasisVector[A] = {
    val cosThetaBy2 = a.fromDouble(rng.nextDouble)
    val sinThetaBy2 = sqrt(1 - cosThetaBy2*cosThetaBy2)
    val phi = 2.0 * pi[A] * rng.nextDouble
    new BasisVector(cosThetaBy2, exp(Complex.i[A] * phi) * sinThetaBy2)
  }
}