package org.waman.worldbrain.qkd

import akka.Done
import akka.actor.Actor
import akka.pattern.pipe
import org.waman.worldbrain.qkd.KeyContainer.RequestKey

import scala.concurrent.Promise

trait KeyContainer{ this: Actor =>

  import context._

  protected def keyLength: Int
  private var currentKey: Seq[Int] = Seq()
  protected def currentKeyBitCount: Int = this.currentKey.length
  protected val keyPromise: Promise[Seq[Int]] = Promise()

  require(keyLength > 0)

  protected def addKeyBits(bits: Seq[Int]): Int = {
    this.currentKey ++= bits
    if(this.currentKey.length >= this.keyLength) {
      this.keyPromise.success(this.currentKey.slice(0, this.keyLength))
      0
    }else{
      this.keyLength - this.currentKey.length
    }
  }

  val getKeyBehavior: Receive = {
    case RequestKey =>
      pipe(this.keyPromise.future) to sender()
    case Done =>
      context stop self
  }
}

object KeyContainer{

  case object RequestKey

  def applyFilter[E](seq: Seq[E], filter: Seq[Int]): Seq[E] =
    (seq zip filter).filter(_._2 == 1).map(_._1)
}