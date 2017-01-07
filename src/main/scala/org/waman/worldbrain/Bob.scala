package org.waman.worldbrain

import akka.actor.Actor

trait Bob[K] extends Actor with KeyContainer[K]{

  override def receive =
    establishKeyBehavior.orElse[Any, Unit](getKeyBehavior)

  def establishKeyBehavior: Receive
}
