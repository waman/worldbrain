package org.waman.worldbrain.system.single


trait StateAlias[F]{

  protected val bases: Seq[StateBasis[F]]

  require(bases.length == 2, "bases Seq must have two elements: "+bases.length)

  protected final def standard: StateBasis[F] = bases.head
  protected final def hadamard: StateBasis[F] = bases(1)

  protected final def zero : StateVector[F] = standard.first
  protected final def one  : StateVector[F] = standard.second
  protected final def plus : StateVector[F] = hadamard.first
  protected final def minus: StateVector[F] = hadamard.second
}
