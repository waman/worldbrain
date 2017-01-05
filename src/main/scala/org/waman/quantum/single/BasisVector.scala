package org.waman.quantum.single

import scala.math._
import spire.math.Complex
import spire.implicits._
import org.waman.quantum.ComplexImplicits._
import org.waman.quantum.Random.nextDouble

case class StateBasis(states: Seq[BasisVector])

case class BasisVector(a: Complex[Double], b: Complex[Double]){

  def *(that: BasisVector): Complex[Double] =
    this.a.conjugate * that.a + this.b.conjugate * that.b

  def probability(that: BasisVector): Double = 
    (this * that).abs**2

  def canonical: BasisVector =
    if(a.isReal){
      val aReal = a.real
      if(aReal > 0)
        this
      else if(aReal < 0)
        BasisVector(-aReal, -b)
      else
        BasisVector(0, b.abs)
    } else {
      val aAbs = a.abs
      val phase = a/aAbs
      BasisVector(aAbs, b/phase)
    }

  /** (theta, phi) */
  def toAnglesOfBlochSphere: (Double, Double) = {
    val v = canonical
    val phi_ = v.b.arg
    val phi = if(phi_ >= 0.0) phi_ else phi_ + 2*Math.PI
    (2*acos(v.a.real), phi)
  }

  def toPointOnBlochSphere: (Double, Double, Double) = ???
  def toComplex: Complex[Double] = ???

  def getPerpendicular: BasisVector =
    BasisVector(b.conjugate, -a.conjugate)

  override def toString: String =
    s"($a)|0> + ($b)|1>"
}

object BasisVector {

  private val c0 = Complex.zero[Double]
  private val c1 = Complex.one[Double]
  private val sqrt2inv = Complex(1.0 / sqrt(2.0))
  // 1/sqrt(2)
  private val i = Complex.i[Double]

  def ofBlochSphere(theta: Double): BasisVector = {
    val thetaBy2 = theta/2.0
    BasisVector(cos(thetaBy2), sin(thetaBy2))
  }

  def ofBlochSphere(theta: Double, phi: Double): BasisVector = {
    val thetaBy2 = theta/2.0
    BasisVector(cos(thetaBy2), (i * phi).exp * sin(thetaBy2))
  }

  def newRandomVectorInReal: BasisVector =
    ofBlochSphere(nextDouble(Math.PI))

  def newRandomVector: BasisVector = {
//    val cosTheta = nextDouble
//    val thetaBy2 = acos(cosTheta)/2.0
//    val phi = nextDouble(Math.PI * 2.0)
//    BasisVector(cos(thetaBy2), (i * phi).exp * sin(thetaBy2))
    val cosThetaBy2 = nextDouble
    val sinThetaBy2 = sqrt(1 - cosThetaBy2*cosThetaBy2)
    val phi = nextDouble(Math.PI * 2.0)
    BasisVector(cosThetaBy2, (i * phi).exp * sinThetaBy2)
  }

  object Zero extends BasisVector(c1, c0){
    override def toString = "|0>"
  }

  object One extends BasisVector(c0, c1){
    override def toString = "|1>"
  }

  object Plus extends BasisVector(sqrt2inv, sqrt2inv){
    override def toString = "|+>"
  }

  object Minus extends BasisVector(sqrt2inv, -sqrt2inv){
    override def toString = "|->"
  }

  object PlusImaginary extends BasisVector(sqrt2inv, sqrt2inv*i){
    override def toString = "|i>"
  }

  object MinusImaginary extends BasisVector(sqrt2inv, -sqrt2inv*i){
    override def toString = "|-i>"
  }


  object Standard extends StateBasis(Seq(Zero, One))
  object Hadamard extends StateBasis(Seq(Plus, Minus))
  object Imaginary extends StateBasis(Seq(PlusImaginary, MinusImaginary))

  def getBasis(state: BasisVector): StateBasis = state match {
    case Zero | One => Standard
    case Plus | Minus => Hadamard
    case PlusImaginary | MinusImaginary => Imaginary
    case _ =>
      StateBasis(Seq(state, state.getPerpendicular))
  }
}