package org.waman.worldbrain.system

trait StateAlias[A]{

  protected val bases: Seq[StateBasis[A]]

  require(bases.length == 2, "bases Seq must have two elements: "+bases.length)

  protected final def standard: StateBasis[A] = bases.head
  protected final def hadamard: StateBasis[A] = bases(1)

  protected final def zero : StateVector[A] = standard.first
  protected final def one  : StateVector[A] = standard.second
  protected final def plus : StateVector[A] = hadamard.first
  protected final def minus: StateVector[A] = hadamard.second
}
