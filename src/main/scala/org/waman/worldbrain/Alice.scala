package org.waman.worldbrain

import akka.actor.Actor

trait Alice extends Actor with KeyContainer{

  override def receive: Receive =
    establishKeyBehavior.orElse[Any, Unit](getKeyBehavior)

  def establishKeyBehavior: Receive
}
