package org.waman.worldbrain

object Protocol {

  class ClassicalBitMessage(val bits: Seq[Int])

  /** This class may be extended by case class*/
  class QubitMessage[Q](qubits: Seq[Q])

  trait Request
}
