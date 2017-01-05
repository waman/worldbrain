package org.waman.quantum.single

import org.scalatest.FreeSpec

import BasisVector._

class QubitSpec extends FreeSpec{

  "observe method" - {

    "return the same state permanently" in {
      val qubit = new Qubit(Zero)

      for(i <- 0 to 10){
        val result = qubit.observe(Standard)
        assert( result == Zero )
      }
    }

    "return" in {
      val n = 10000
      var nPlus = 0
      for(i <- 1 to n){
        val qubit = new Qubit(Zero)
        val result = qubit.observe(Hadamard)
        if(result == Plus) nPlus += 1
      }
      println(nPlus)
    }
  }
}
