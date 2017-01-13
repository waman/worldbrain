package org.waman.worldbrain

import akka.actor.Actor

trait Bob extends Actor with KeyContainer{

  override def receive =
    establishKeyBehavior.orElse[Any, Unit](getKeyBehavior)

  def establishKeyBehavior: Receive
}
