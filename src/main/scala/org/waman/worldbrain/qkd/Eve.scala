package org.waman.worldbrain.qkd

import akka.actor.{Actor, ActorRef}

abstract class Eve(protected val alice: ActorRef,
                      protected val bob: ActorRef)
    extends Actor with KeyContainer{

  override def receive: Receive =
    eavesdropBehavior
      .orElse[Any, Unit](getKeyBehavior)
      .orElse(defaultEavesdropBehavior)

  def defaultEavesdropBehavior: Receive = {
    case x =>
      sender() match {
        case a if a == alice => bob ! x
        case b if b == bob   => alice ! x
      }
  }

  def eavesdropBehavior: Receive
}
