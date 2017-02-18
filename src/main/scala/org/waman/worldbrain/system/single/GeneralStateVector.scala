package org.waman.worldbrain.system.single

import org.waman.worldbrain.system
import org.waman.worldbrain.system.Tolerance
import org.waman.worldbrain.system.single.StateVector.StateVectorImpl
import spire.algebra._
import spire.math._
import spire.implicits._

abstract class GeneralStateVector[F](implicit f: Fractional[F], trig: Trig[F])
  extends system.StateVector{ lhs =>

  def a: Complex[F]
  def b: Complex[F]
  def norm: F

  def normalized: StateVector[F]

  def probability(rhs: GeneralStateVector[F]): F =
    lhs.normalized probability rhs.normalized

  //***** Complex vector algebra *****
  def *(rhs: GeneralStateVector[F]): Complex[F] =
    lhs.a.conjugate * rhs.a + lhs.b.conjugate * rhs.b

  /** (theta, phi) */
  def toAnglesOfBlochSphere: (F, F)

  def toPointOnBlochSphere: (F, F, F)

  /** Return null if (0, 0, 1) (infinity) */
  def toComplex: Complex[F]

  def toTuple: (Complex[F], Complex[F]) = (a, b)

  def getPerpendicular: StateVector[F]

  //***** equivalency *****
  def hasTheSameCoefficientsAs(rhs: GeneralStateVector[F])(implicit t: Tolerance[F]): Boolean =
    t.test(lhs.a, rhs.a) && t.test(lhs.b, rhs.b)

  def isTheSameStateAs(rhs: GeneralStateVector[F])(implicit t: Tolerance[F]): Boolean =
    t.test(lhs probability rhs, f.one)

  override def toString: String = s"$a|0> + $b|1>"
}

object GeneralStateVector{

  private class GeneralStateVectorImpl[F](val a: Complex[F], val b: Complex[F])
                                         (implicit f: Fractional[F], trig: Trig[F], tlr: Tolerance[F])
    extends GeneralStateVector[F]{ lhs =>

    override lazy val norm = sqrt((this * this).real)

    override def normalized = StateVector(a, b)

    /** (theta, phi) */
    override def toAnglesOfBlochSphere: (F, F) =
      normalized.toAnglesOfBlochSphere

    override def toPointOnBlochSphere: (F, F, F) =
      normalized.toPointOnBlochSphere

    /** Return null if (0, 0, 1) (infinity) */
    override def toComplex: Complex[F] =
      normalized.toComplex

    override def getPerpendicular: StateVector[F] =
      normalized.getPerpendicular

    //***** equivalency *****

    override def toString: String =
      s"($a)|0> + ($b)|1>"
  }
  
  //********** apply factory method **********
  def apply[F](a: Complex[F], b: Complex[F])
              (implicit f: Fractional[F], trig: Trig[F], tlr: Tolerance[F]): GeneralStateVector[F] =
    a match {
      case _ if a.isReal => apply(a.real, b)
      case _ => new GeneralStateVectorImpl(a, b)
    }

  def apply[F](a: F, b: Complex[F])
              (implicit f: Fractional[F], trig: Trig[F], tlr: Tolerance[F]): GeneralStateVector[F] =
    if(a > 0) {
      val norm2 = a * a + (b.conjugate * b).real
      if (tlr.test(norm2, f.one))
        new StateVectorImpl(a, b)  // guaranteed to be normalized
      else
        new GeneralStateVectorImpl(Complex(a), b)

    }else if(a < 0) {
      new GeneralStateVectorImpl(Complex(a), b)

    }else{
      if(b.isReal && b.real.isOne) // maybe unnecessary to evaluate by tolerance
        new StateVectorImpl(f.zero, Complex.one)  // guaranteed to be normalized
      else
        new GeneralStateVectorImpl(Complex(a), b)
    }
}

trait StateVectorInstances[F] extends InnerProductSpace[StateVector[F], F]