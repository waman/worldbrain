package org.waman.quantum.single

import org.waman.quantum.Random._

class Qubit(private var state: BasisVector){

  def observe(basis: StateBasis): BasisVector = synchronized{
    val v = basis.states.head
    val p = this.state probability v
    if(nextDouble < p)
      this.state = v
    else
      this.state = basis.states(1)

    this.state
  }
}
