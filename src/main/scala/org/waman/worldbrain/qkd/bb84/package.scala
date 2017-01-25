package org.waman.worldbrain.qkd

import org.waman.worldbrain.qkd.KeyContainer._
import org.waman.worldbrain.system.single.BasisKet._
import org.waman.worldbrain.system.single.StateBasis._
import org.waman.worldbrain.system.single.{BasisKet, StateBasis}

package object bb84 {

  private[bb84] def extractKey(states: Seq[BasisKet], filter: Seq[Int]): Seq[Int] =
    applyFilter(states, filter).map(encode)

  private[bb84] def encode(state: BasisKet): Int = state match {
    case Zero | Plus => 0
    case One  | Minus => 1
    case _ => throw new IllegalArgumentException("Unknown state: "+state)
  }

  private[bb84] def decode(bit: Int, basis: StateBasis): BasisKet =
    basis match {
      case Standard => if(bit == 0) Zero else One
      case Hadamard => if(bit == 0) Plus else Minus
      case _        => throw new IllegalArgumentException("Unknown basis: "+basis)
    }

  private[bb84] def encode(basis: StateBasis): Int = basis match {
    case Standard => 0
    case Hadamard => 1
    case _        => throw new IllegalArgumentException("Unknown basis: "+basis)
  }

  private[bb84] def decode(bit: Int): StateBasis = bit match {
    case 0 => Standard
    case 1 => Hadamard
  }
}