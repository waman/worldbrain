package org.waman.worldbrain.system

import spire.algebra.Field

package object single {

  import org.waman.worldbrain.system.{single => s}

  def zero[A](implicit a: Field[A], ss: StateSpace[A]): s.StateVector[A] =
    ss.zero

  def one[A](implicit a: Field[A], ss: StateSpace[A]): s.StateVector[A] =
    ss.one

  def plus[A](implicit a: Field[A], ss: StateSpace[A]): s.StateVector[A] =
    ss.plus

  def minus[A](implicit a: Field[A], ss: StateSpace[A]): s.StateVector[A] =
    ss.minus

  def iPlus[A](implicit a: Field[A], ss: StateSpace[A]): s.StateVector[A] =
    ss.iPlus

  def iMinus[A](implicit a: Field[A], ss: StateSpace[A]): s.StateVector[A] =
    ss.iMinus

  def standard[A](implicit a: Field[A], ss: StateSpace[A]): s.StateBasis[A] =
    ss.standard

  def hadamard[A](implicit a: Field[A], ss: StateSpace[A]): s.StateBasis[A] =
    ss.hadamard
}
