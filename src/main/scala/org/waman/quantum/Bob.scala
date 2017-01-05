package org.waman.quantum

import akka.actor.Actor

trait Bob[K] extends Actor with KeyContainer[K]{

  override def receive =
    establishKeyBehavior.orElse[Any, Unit](getKeyBehavior)

  def establishKeyBehavior: Receive
}
