package org.waman.worldbrain.system

import spire.implicits._
import spire.math.{Complex, Real}
import spire.algebra._

class Tolerance[F](value: F)
                  (implicit f: Field[F], o: IsReal[F], n: NRoot[F]){

  def test(x: F):Boolean = x <= value
  def test(x: F, y: F): Boolean = test(x - y)

  def test(x: Complex[F]): Boolean = x.abs <= value
  def test(x: Complex[F], y: Complex[F]): Boolean = test(x - y)
}

object Tolerance{

  implicit val floatTolerance: Tolerance[Float] = new Tolerance(1e-5f)
  implicit val doubleTolerance: Tolerance[Double] = new Tolerance(1e-10)
  implicit val bigDecimalTolerance: Tolerance[BigDecimal] = new Tolerance(0)
  implicit val realTolerance: Tolerance[Real] = new Tolerance(0)
}