package org.waman.worldbrain.system

package object single {

  import org.waman.worldbrain.system.{single => s}

  def zero[A](implicit a: Fractional[A], ss: StateSpace[A]): s.BasisVector[A] =
    ss.zero

  def one[A](implicit a: Fractional[A], ss: StateSpace[A]): s.BasisVector[A] =
    ss.one

  def plus[A](implicit a: Fractional[A], ss: StateSpace[A]): s.BasisVector[A] =
    ss.plus

  def minus[A](implicit a: Fractional[A], ss: StateSpace[A]): s.BasisVector[A] =
    ss.minus

  def iPlus[A](implicit a: Fractional[A], ss: StateSpace[A]): s.BasisVector[A] =
    ss.iPlus

  def iMinus[A](implicit a: Fractional[A], ss: StateSpace[A]): s.BasisVector[A] =
    ss.iMinus

  def standard[A](implicit a: Fractional[A], ss: StateSpace[A]): s.StateBasis[A] =
    ss.standard

  def hadamard[A](implicit a: Fractional[A], ss: StateSpace[A]): s.StateBasis[A] =
    ss.hadamard
}
