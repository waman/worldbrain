package org.waman.worldbrain.single


import scala.math._

import spire.math.Complex
import spire.implicits._
import spire.random.Generator

import org.waman.worldbrain.single.StateBasis._
import org.waman.worldbrain.ComplexImplicits._
import org.waman.worldbrain.Tolerance

case class BasisVector(a: Complex[Double], b: Complex[Double]){

  def *(that: BasisVector): Complex[Double] =
    this.a.conjugate * that.a + this.b.conjugate * that.b

  def probability(that: BasisVector): Double = {
    val amplitude = (this * that).abs
    amplitude * amplitude
  }

  /** exp(i*GlobalPhase) (not GlobalPhase) */
  def globalPhaseFactor: Complex[Double] = a/a.abs

  def canonical: BasisVector = globalPhaseFactor match {
    case phase if phase.isReal =>
      a.real match {
        case ar if ar > 0 => this
        case ar if ar < 0 => BasisVector(-ar, -b)
        case _            => BasisVector(0, b.abs)
      }

    case phase =>
      BasisVector(a.abs, b/phase)
  }

  /** (theta, phi) */
  def toAnglesOfBlochSphere: (Double, Double) = {
    val v = canonical
    val phi_ = v.b.arg
    val phi = if(phi_ >= 0.0) phi_ else phi_ + 2*Math.PI
    (2*acos(v.a.real), phi)
  }

  def toPointOnBlochSphere: (Double, Double, Double) = {
    val (theta, phi) = toAnglesOfBlochSphere
    val sinTheta = sin(theta)
    (sinTheta*cos(phi), sinTheta*sin(phi), cos(theta))
  }

  def toComplex: Complex[Double] = toPointOnBlochSphere match {
    case (0, 0, 1) => Complex(Double.PositiveInfinity, Double.PositiveInfinity)
    case (x, y, z) =>
      val denom = 1-z
      Complex(x/denom, y/denom)
  }

  def getPerpendicular: BasisVector =
    BasisVector(b.conjugate, -a.conjugate)

  //***** equivalency *****
  def hasTheSameCoefficientsAs(that: BasisVector)(implicit t: Tolerance): Boolean =
    t.test(a, that.a) && t.test(b, that.b)

  def isTheSameStateAs(that: BasisVector)(implicit t: Tolerance): Boolean =
    t.test((this * that).abs, 1.0)

  override def toString: String =
    s"($a)|0> + ($b)|1>"
}

object BasisVector {

  private val sqrt2inv: Complex[Double] = Complex(1.0/sqrt(2))
  private val i: Complex[Double] = Complex.i[Double]

  def ofBlochSphere(theta: Double): BasisVector = {
    val thetaBy2 = theta/2.0
    BasisVector(cos(thetaBy2), sin(thetaBy2))
  }

  def ofBlochSphere(theta: Double, phi: Double): BasisVector = {
    val thetaBy2 = theta/2.0
    BasisVector(cos(thetaBy2), (i * phi).exp * sin(thetaBy2))
  }

  def newRandomVectorInReal(rng: Generator): BasisVector =
    ofBlochSphere(rng.nextDouble(Math.PI))

  def newRandomVector(rng: Generator): BasisVector = {
//    val cosTheta = nextDouble
//    val thetaBy2 = acos(cosTheta)/2.0
//    val phi = nextDouble(Math.PI * 2.0)
//    BasisVector(cos(thetaBy2), (i * phi).exp * sin(thetaBy2))
    val cosThetaBy2 = rng.nextDouble
    val sinThetaBy2 = sqrt(1 - cosThetaBy2*cosThetaBy2)
    val phi = rng.nextDouble(Math.PI * 2.0)
    BasisVector(cosThetaBy2, (i * phi).exp * sinThetaBy2)
  }

  def getBasis(state: BasisVector): StateBasis = state match {
    case Zero | One => Standard
    case Plus | Minus => Hadamard
    case PlusImaginary | MinusImaginary => Imaginary
    case _ =>
      StateBasis(Seq(state, state.getPerpendicular))
  }

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
}