package org.waman.worldbrain.qkd.bb84

import org.waman.worldbrain.qkd.applyFilter
import org.waman.worldbrain.system.single.toy.StateBasis._
import org.waman.worldbrain.system.single.toy.StateVector._
import org.waman.worldbrain.system.single.toy._
import spire.random.Generator

package object toy {

  def createRandomBases(rng: Generator, n: Int): Seq[StateBasis] =
    (0 until n).map { _ =>
      if (rng.nextBoolean) Standard
      else                 Hadamard
    }

  def extractKey(states: Seq[StateVector], filter: Seq[Int]): Seq[Int] =
    applyFilter(states, filter).map(encode)

  def encode(state: StateVector): Int = state match {
    case Zero | Plus => 0
    case One  | Minus => 1
  }

  def decode(bit: Int, basis: StateBasis): StateVector =
    basis match {
      case Standard => if(bit == 0) Zero else One
      case Hadamard => if(bit == 0) Plus else Minus
    }

  def encode(basis: StateBasis): Int = basis match {
    case Standard => 0
    case Hadamard => 1
  }

  def decode(bit: Int): StateBasis = bit match {
    case 0 => Standard
    case 1 => Hadamard
  }
}
