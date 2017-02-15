package org.waman.worldbrain.system.single


trait StateAlias[A]{

  protected val bases: Seq[StateBasis[A]]

  protected final def standard: StateBasis[A] = bases.head
  protected final def hadamard: StateBasis[A] = bases(1)

  protected final def zero : BasisVector[A] = standard.first
  protected final def one  : BasisVector[A] = standard.second
  protected final def plus : BasisVector[A] = hadamard.first
  protected final def minus: BasisVector[A] = hadamard.second
}
