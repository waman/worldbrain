package org.waman.worldbrain.system.single.toy

import org.waman.worldbrain.system
import spire.random.Generator

class Qubit(private var state: StateVector)
  extends system.Qubit[StateVector, StateBasis]{

  override def observe(basis: StateBasis)(implicit rng: Generator): StateVector =
    synchronized{
      if(basis.contains(this.state))
        this.state
      else{
        if(rng.nextBoolean) basis.states.head
        else                basis.states(1)
      }
    }
}
