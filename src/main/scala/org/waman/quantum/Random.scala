package org.waman.quantum

import spire.random.rng.MersenneTwister32


object Random {

  private val rng = MersenneTwister32().sync

  def nextBoolean: Boolean = rng.nextBoolean
  def nextInt(n: Int): Int = rng.nextInt(n)
  def nextDouble: Double = rng.nextDouble()
  def nextDouble(d: Double): Double = rng.nextDouble(d)
}
