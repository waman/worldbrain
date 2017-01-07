package org.waman.worldbrain

import akka.actor.Actor

trait Alice[K] extends Actor with KeyContainer[K]{

  override def receive: Receive =
    establishKeyBehavior.orElse[Any, Unit](getKeyBehavior)

  def establishKeyBehavior: Receive
}
