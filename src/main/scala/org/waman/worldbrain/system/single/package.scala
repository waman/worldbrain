package org.waman.worldbrain.system

import spire.algebra.Field

package object single {

  import org.waman.worldbrain.system.{single => s}

  def zero[F](implicit a: Field[F], ss: StateSpace[F]): s.StateVector[F] =
    ss.zero

  def one[F](implicit a: Field[F], ss: StateSpace[F]): s.StateVector[F] =
    ss.one

  def plus[F](implicit a: Field[F], ss: StateSpace[F]): s.StateVector[F] =
    ss.plus

  def minus[F](implicit a: Field[F], ss: StateSpace[F]): s.StateVector[F] =
    ss.minus

  def iPlus[F](implicit a: Field[F], ss: StateSpace[F]): s.StateVector[F] =
    ss.iPlus

  def iMinus[F](implicit a: Field[F], ss: StateSpace[F]): s.StateVector[F] =
    ss.iMinus

  def standard[F](implicit a: Field[F], ss: StateSpace[F]): s.StateBasis[F] =
    ss.standard

  def hadamard[F](implicit a: Field[F], ss: StateSpace[F]): s.StateBasis[F] =
    ss.hadamard
}
