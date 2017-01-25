package org.waman.worldbrain

import akka.actor.ActorRef
import org.waman.worldbrain.system.single.Qubit

object Protocol {

  case class EstablishKey(bob: ActorRef)
  class ClassicalBitMessage(val bits: Seq[Int])
  class QubitMessage(qubits: Seq[Qubit])
  trait Request
}
