package org.waman.worldbrain.qkd

import akka.actor.Actor


trait Alice extends Actor with KeyContainer{

  override def receive: Receive =
    establishKeyBehavior.orElse[Any, Unit](getKeyBehavior)

  def establishKeyBehavior: Receive
}

object Alice{

  case object RequestLog
}