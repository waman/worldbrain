package org.waman.worldbrain.system

import spire.algebra._
import spire.implicits._
import spire.math._

abstract class GeneralStateVector[A: Fractional: Trig]{ lhs =>

  def a: Complex[A]
  def b: Complex[A]
  def norm: A

  def normalized: StateVector[A]

  def probability(rhs: GeneralStateVector[A]): A =
    lhs.normalized probability rhs.normalized

  //***** Complex vector algebra *****
  def eqv(rhs: GeneralStateVector[A]): Boolean =
    lhs.a === rhs.a && lhs.b === rhs.b

  def neqv(rhs: GeneralStateVector[A]): Boolean =
    lhs.a =!= rhs.a || lhs.b =!= rhs.b

  def unary_- : GeneralStateVector[A] =
    GeneralStateVector(-a, -b)

  def +(rhs: GeneralStateVector[A]): GeneralStateVector[A] =
    GeneralStateVector(lhs.a + rhs.b, lhs.b + rhs.b)

  def *(x: Complex[A]): GeneralStateVector[A] =
    GeneralStateVector(lhs.a * x, lhs.b * x)

  def *(rhs: GeneralStateVector[A]): Complex[A] =
    lhs.a.conjugate * rhs.a + lhs.b.conjugate * rhs.b

  /** (theta, phi) */
  def toAnglesOfBlochSphere: (A, A)

  def toPointOnBlochSphere: (A, A, A)

  /** Return null if (0, 0, 1) (infinity) */
  def toComplex: Complex[A]

  def toTuple: (Complex[A], Complex[A]) = (a, b)

  def getPerpendicular: StateVector[A]

  //***** equivalency *****
  def hasTheSameCoefficientsAs(rhs: GeneralStateVector[A])(implicit t: Tolerance[A]): Boolean =
    t.test(lhs.a, rhs.a) && t.test(lhs.b, rhs.b)

  def isTheSameStateAs(rhs: GeneralStateVector[A])(implicit t: Tolerance[A]): Boolean =
    t.test(lhs probability rhs, 1)

  override def toString: String = s"$a|0> + $b|1>"
}

object GeneralStateVector extends GeneralStateVectorInstances{

  private class GeneralStateVectorImpl[A: Fractional: Trig]
      (val a: Complex[A], val b: Complex[A])
      extends GeneralStateVector[A]{ lhs =>

    override lazy val norm = sqrt((this * this).real)

    override def normalized = StateVector(a, b)

    /** (theta, phi) */
    override def toAnglesOfBlochSphere: (A, A) =
      normalized.toAnglesOfBlochSphere

    override def toPointOnBlochSphere: (A, A, A) =
      normalized.toPointOnBlochSphere

    /** Return null if (0, 0, 1) (infinity) */
    override def toComplex: Complex[A] =
      normalized.toComplex

    override def getPerpendicular: StateVector[A] =
      normalized.getPerpendicular
  }
  
  //********** apply factory method **********
  def apply[A: Fractional: Trig](a: Complex[A], b: Complex[A]): GeneralStateVector[A] =
    new GeneralStateVectorImpl(a, b)
}

trait GeneralStateVectorInstances{

  implicit def GeneralStateVectorSpace[A: Fractional: Trig]: GeneralStateVectorAlgebra[A] =
    new GeneralStateVectorAlgebra[A]

  implicit def GeneralStateVectorSpaceEq[A: Fractional: Trig]: GeneralStateVectorEq[A] =
    new GeneralStateVectorEq[A]
}

private[system] class GeneralStateVectorAlgebra[A: Fractional: Trig]
  extends InnerProductSpace[GeneralStateVector[A], Complex[A]]{

  override implicit def scalar = implicitly[Field[Complex[A]]]
  override val zero = GeneralStateVector(scalar.zero, scalar.zero)

  override def negate(v: GeneralStateVector[A]) = -v
  override def plus(v: GeneralStateVector[A], w: GeneralStateVector[A]) = v + w
  override def timesl(x: Complex[A], v: GeneralStateVector[A]) = v * x
  override def dot(v: GeneralStateVector[A], w: GeneralStateVector[A]) = v * w
}

private[system] class GeneralStateVectorEq[A: Fractional: Trig] extends Eq[GeneralStateVector[A]]{
  override def eqv(x: GeneralStateVector[A], y: GeneralStateVector[A]) = x eqv y
  override def neqv(x: GeneralStateVector[A], y: GeneralStateVector[A]) = x neqv y
}