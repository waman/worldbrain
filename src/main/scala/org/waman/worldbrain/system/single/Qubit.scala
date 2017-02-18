package org.waman.worldbrain.system.single

import org.waman.worldbrain.system
import spire.math.Fractional
import spire.implicits._
import spire.random.Generator

abstract class Qubit[F: Fractional] extends system.Qubit[StateVector[F], StateBasis[F]]

object Qubit{

  private class QubitImpl[F](private var state: StateVector[F])
                            (implicit f: Fractional[F])
    extends Qubit[F]{

    override def observe(basis: StateBasis[F])(implicit rng: Generator): StateVector[F] =
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


  def apply[F](state: StateVector[F])(implicit f: Fractional[F]): Qubit[F] =
    new QubitImpl(state)
}