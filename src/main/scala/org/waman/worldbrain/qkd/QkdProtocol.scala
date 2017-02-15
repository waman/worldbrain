package org.waman.worldbrain.qkd

import akka.actor.ActorRef

object QkdProtocol {
  case class EstablishKey(bob: ActorRef)
  case object RequestKey
}
