package org.waman.worldbrain

import org.waman.worldbrain.system.Qubit

object Protocol {

  class ClassicalBitMessage(val bits: Seq[Int])

  /** This class may be extended by case class*/
  class QubitMessage[Q <: Qubit[_, _]](qubits: Seq[Q])

  trait Request
}
