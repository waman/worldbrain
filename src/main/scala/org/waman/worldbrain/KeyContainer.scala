package org.waman.worldbrain

import scala.concurrent.Promise

import akka.Done
import akka.actor.Actor
import akka.pattern.pipe
import org.waman.worldbrain.KeyContainer.RequestKey

trait KeyContainer[K] { this: Actor =>

  import context._

  protected val keyPromise: Promise[K] = Promise()

  protected def setKey(key: K): Unit = this.keyPromise.success(key)

  val getKeyBehavior: Receive = {
    case RequestKey =>
      pipe(this.keyPromise.future) to sender()
    case Done =>
      context stop self
  }
}

object KeyContainer{

  final case object RequestKey
}