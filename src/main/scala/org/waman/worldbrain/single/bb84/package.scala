package org.waman.worldbrain.single

import org.waman.worldbrain.single.BasisVector._
import org.waman.worldbrain.single.StateBasis._
import spire.random.Generator

package object bb84 {

  private[bb84] def createRandomBases(rng: Generator, n: Int): Seq[StateBasis] =
    (0 until n).map{ i =>
      if(rng.nextBoolean) Standard
      else                Hadamard
    }

  private[bb84] def extractKey(states: Seq[BasisVector], filter: Seq[Int]): Seq[Int] =
    applyFilter(states, filter).map(encode)

  private[bb84] def applyFilter[E](seq: Seq[E], filter: Seq[Int]): Seq[E] =
    (seq zip filter).filter(_._2 == 1).map(_._1)

  private[bb84] def encode(state: BasisVector): Int = state match {
    case Zero | Plus => 0
    case One  | Minus => 1
  }

  private[bb84] def decode(bit: Int, basis: StateBasis): BasisVector =
    basis match {
      case Standard => if(bit == 0) Zero else One
      case Hadamard => if(bit == 0) Plus else Minus
    }

  private[bb84] def encode(basis: StateBasis): Int = basis match {
    case Standard => 0
    case Hadamard => 1
  }

  private[bb84] def decode(bit: Int): StateBasis = bit match {
    case 0 => Standard
    case 1 => Hadamard
  }
}
