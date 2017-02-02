package org.waman.worldbrain.system.single

import scala.math._
import spire.math.Complex
import spire.implicits._
import spire.random.Generator
import org.waman.worldbrain.system.single.StateBasis._
import org.waman.worldbrain.system.Tolerance
import org.waman.worldbrain.ComplexImplicits._

class StateVector private(val a: Double, val b: Complex[Double]){

  require(StateVector.normalizationTolerance.test(a*a + b.conjugate*b, 1))

  def *(that: StateVector): Complex[Double] =
    this.a * that.a + this.b.conjugate * that.b  // a is real

  def probability(that: StateVector): Double = {
    val amplitude = (this * that).abs
    amplitude * amplitude
  }

  /** (theta, phi) */
  def toAnglesOfBlochSphere: (Double, Double) = {
    val phi_ = b.arg
    val phi = if(phi_ >= 0.0) phi_ else phi_ + 2*Math.PI
    (2*acos(a), phi)
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

  def toTuple: (Complex[Double], Complex[Double]) = (a, b)

  def getPerpendicular: StateVector = {
    val bAbs = b.abs
    val phase = b/bAbs
    new StateVector(bAbs, -a*phase)
    // (b*, -a) == (|b|e^(-i phi), -a) == (|b|, -a e^(i phi)
  }

  //***** equivalency *****
  def hasTheSameCoefficientsAs(that: StateVector)(implicit t: Tolerance): Boolean =
    t.test(a, that.a) && t.test(b, that.b)

  def isTheSameStateAs(that: StateVector)(implicit t: Tolerance): Boolean =
    t.test((this * that).abs, 1.0)

  def symbol: String = toTuple.toString

  override def toString: String =
    s"($a)|0> + ($b)|1>"
}

object StateVector {

  private[StateVector] val normalizationTolerance = Tolerance(1e-10)

  def normalized(a: Complex[Double], b: Complex[Double]): StateVector = {
    val norm = (a.conjugate * a + b.conjugate * b).sqrt
    apply(a/norm, b/norm)
  }

  def apply(a: Complex[Double], b: Complex[Double]): StateVector = {
    val aAbs = a.abs
    val phase = a/aAbs
    new StateVector(aAbs, b/phase)
  }

  private val sqrt2inv: Double = 1.0/sqrt(2)
  private val i: Complex[Double] = Complex.i[Double]

  def ofBlochSphere(theta: Double): StateVector = {
    val thetaBy2 = theta/2.0
    new StateVector(cos(thetaBy2), sin(thetaBy2))
  }

  def ofBlochSphere(theta: Double, phi: Double): StateVector = {
    val thetaBy2 = theta/2.0
    new StateVector(cos(thetaBy2), (i * phi).exp * sin(thetaBy2))
  }

  def newRandomVectorInReal(rng: Generator): StateVector =
    ofBlochSphere(rng.nextDouble(Math.PI))

  def newRandomVector(rng: Generator): StateVector = {
//    val cosTheta = nextDouble
//    val thetaBy2 = acos(cosTheta)/2.0
//    val phi = nextDouble(Math.PI * 2.0)
//    BasisVector(cos(thetaBy2), (i * phi).exp * sin(thetaBy2))
    val cosThetaBy2 = rng.nextDouble
    val sinThetaBy2 = sqrt(1 - cosThetaBy2*cosThetaBy2)
    val phi = rng.nextDouble(Math.PI * 2.0)
    new StateVector(cosThetaBy2, (i * phi).exp * sinThetaBy2)
  }

  def getBasis(state: StateVector): StateBasis = state match {
    case Zero | One => Standard
    case Plus | Minus => Hadamard
    case PlusImaginary | MinusImaginary => Imaginary
    case _ =>
      StateBasis(Seq(state, state.getPerpendicular))
  }

  object Zero extends StateVector(1, 0){
    override def symbol: String = "0"
    override def toString: String = "|0>"
  }

  object One extends StateVector(0, 1){
    override def symbol: String = "1"
    override def toString: String = "|1>"
  }

  object Plus extends StateVector(sqrt2inv, sqrt2inv){
    override def symbol: String = "+"
    override def toString: String = "|+>"
  }

  object Minus extends StateVector(sqrt2inv, -sqrt2inv){
    override def symbol: String = "-"
    override def toString: String = "|->"
  }

  object PlusImaginary extends StateVector(sqrt2inv, i*sqrt2inv){
    override def symbol: String = "i"
    override def toString: String = "|i>"
  }

  object MinusImaginary extends StateVector(sqrt2inv, -i*sqrt2inv){
    override def symbol: String = "i"
    override def toString: String = "|-i>"
  }
}