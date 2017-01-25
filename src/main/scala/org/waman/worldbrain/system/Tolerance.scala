package org.waman.worldbrain.system

import spire.implicits._
import spire.math.Complex

import scala.math._

case class Tolerance(value: Double){

  def test(x: Double):Boolean = abs(x) <= value
  def test(x: Double, y: Double): Boolean = test(x - y)

  def test(x: Complex[Double]): Boolean = x.abs <= value
  def test(x: Complex[Double], y: Complex[Double]): Boolean = test(x - y)
}
