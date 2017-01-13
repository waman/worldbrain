package org.waman.worldbrain.single

import akka.actor.ActorRef

object Protocol {

  case class EstablishKey(bob: ActorRef)
  class ClassicalBitMessage(val bits: Seq[Int])
  class QubitMessage(qubits: Seq[Qubit])
  trait Request
}
