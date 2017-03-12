package org.waman.worldbrain.system

import org.waman.worldbrain.WorldbrainCustomSpec

class QubitSpec extends WorldbrainCustomSpec{

  "observe method" - {

    "return the same state permanently" in {
      import org.waman.worldbrain.system.StateSpace.doubleStateSpace._

      val qubit = Qubit(zero)

      for(i <- 0 to 10){
        val result = qubit.observe(standard)
        assert( result == zero )
      }
    }

    "return" in {
      import org.waman.worldbrain.system.StateSpace.doubleStateSpace._

      val n = 10000
      var nPlus = 0
      for(i <- 1 to n){
        val qubit = Qubit(zero)
        val result = qubit.observe(hadamard)
        if(result == plus) nPlus += 1
      }
      println(nPlus)
    }
  }
}
