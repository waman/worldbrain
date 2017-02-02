package org.waman.worldbrain.system.single

import spire.random.Generator

class Qubit(private var state: StateVector){

  def observe(basis: StateBasis)(implicit rng: Generator): StateVector = synchronized{
    val v = basis.states.head
    val p = this.state probability v
    if(rng.nextDouble < p)
      this.state = v
    else
      this.state = basis.states(1)

    this.state
  }
}
