package org.waman.worldbrain.system

import spire.implicits._
import spire.math.Complex
import spire.algebra._

case class Tolerance[A](value: A)
                       (implicit f: Field[A], o: IsReal[A], n: NRoot[A]){

  def test(x: A):Boolean = x <= value
  def test(x: A, y: A): Boolean = test(x - y)

  def test(x: Complex[A]): Boolean = x.abs <= value
  def test(x: Complex[A], y: Complex[A]): Boolean = test(x - y)
}
