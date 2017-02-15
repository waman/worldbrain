package org.waman.worldbrain.system.single

import org.waman.worldbrain.WorldbrainCustomSpec

class QubitSpec extends WorldbrainCustomSpec{

  "observe method" - {

    "return the same state permanently" in {
      import StateSpace.DoubleStateSpace._

      val qubit = new Qubit(zero)

      for(i <- 0 to 10){
        val result = qubit.observe(standard)
        assert( result == zero )
      }
    }

    "return" in {
      import StateSpace.DoubleStateSpace._

      val n = 10000
      var nPlus = 0
      for(i <- 1 to n){
        val qubit = new Qubit(zero)
        val result = qubit.observe(hadamard)
        if(result == plus) nPlus += 1
      }
      println(nPlus)
    }
  }
}
