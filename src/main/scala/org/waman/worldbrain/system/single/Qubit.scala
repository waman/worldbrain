package org.waman.worldbrain.system.single

import org.waman.worldbrain.system
import spire.math._
import spire.implicits._
import spire.random.Generator

class Qubit[A: Fractional](private var state: BasisVector[A])
    extends system.Qubit[BasisVector[A], StateBasis[A]]{

  override def observe(basis: StateBasis[A])(implicit rng: Generator): BasisVector[A] =
    synchronized{
      val v = basis.first
      val p = this.state probability v
      if(rng.nextDouble < p)
        this.state = v
      else
        this.state = basis.second

      this.state
    }
}
