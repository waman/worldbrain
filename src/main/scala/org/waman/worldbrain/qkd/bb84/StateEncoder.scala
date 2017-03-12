package org.waman.worldbrain.qkd.bb84

import org.waman.worldbrain.qkd.applyFilter
import org.waman.worldbrain.system.{StateAlias, StateBasis, StateVector}
import spire.random.Generator

trait StateEncoder[A] extends StateAlias[A]{

  def getBasis(state: StateVector[A]): StateBasis[A] =
    if(standard.contains(state))
      standard
    else
      hadamard

  def createRandomBases(n: Int)(implicit rng: Generator): Seq[StateBasis[A]] =
    (0 until n).map(_ => rng.chooseFromSeq(bases))

  def extractKey(states: Seq[StateVector[A]], filter: Seq[Int]): Seq[Int] =
    applyFilter(states, filter).map(encodeState)

  def encodeState(state: StateVector[A]): Int =
    state match {
      case s if s == zero || s == plus  => 0
      case s if s == one  || s == minus => 1
    }

  def decodeState(bit: Int, basis: StateBasis[A]): StateVector[A] = basis.states(bit)

  def encodeBasis(basis: StateBasis[A]): Int =
    basis match {
      case b if b == standard => 0
      case b if b == hadamard => 1
    }

  def decodeBasis(bit: Int): StateBasis[A] = bases(bit)
}
