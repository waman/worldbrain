package org.waman.worldbrain.qkd.bb84.toy

import org.waman.worldbrain.qkd.applyFilter
import org.waman.worldbrain.system.single.toy.StateBasis._
import org.waman.worldbrain.system.single.toy.BasisVector._
import org.waman.worldbrain.system.single.toy.{StateBasis, BasisVector}
import spire.random.Generator

trait StateEncoder{

  def getBasis(state: BasisVector): StateBasis =
    state match {
      case Zero | One   => Standard
      case Plus | Minus => Hadamard
    }

  def createRandomBasisSeq(n: Int)(implicit rng: Generator): Seq[StateBasis] =
    (0 until n).map{ _ =>
      if(rng.nextBoolean)
        StateBasis.Standard
      else
        StateBasis.Hadamard
    }

  def extractKey(states: Seq[BasisVector], filter: Seq[Int]): Seq[Int] =
    applyFilter(states, filter).map(encodeState)

  def encodeState(state: BasisVector): Int =
    state match {
      case Zero | Plus  => 0
      case One  | Minus => 1
    }

  def decodeState(bit: Int, basis: StateBasis): BasisVector = basis.states(bit)

  def encodeBasis(basis: StateBasis): Int =
    basis match {
      case Standard => 0
      case Hadamard => 1
    }

  def decodeBasis(bit: Int): StateBasis =
    bit match {
      case 0 => Standard
      case 1 => Hadamard
    }
}
