package org.waman.worldbrain

import akka.Done
import akka.actor.Actor
import akka.pattern.pipe
import org.waman.worldbrain.KeyContainer.RequestKey

import scala.concurrent.Promise

trait KeyContainer{ this: Actor =>

  import context._

  protected def bitLength: Int
  private var keyBits: Seq[Int] = Seq()
  protected val keyPromise: Promise[Seq[Int]] = Promise()

  protected def addKeyBits(bits: Seq[Int]): Int = {
    this.keyBits ++= bits
    if(this.keyBits.length >= this.bitLength) {
      this.keyPromise.success(this.keyBits.slice(0, this.bitLength))
      0
    }else{
      this.bitLength - this.keyBits.length
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

  final case object RequestKey

  def applyFilter[E](seq: Seq[E], filter: Seq[Int]): Seq[E] =
    (seq zip filter).filter(_._2 == 1).map(_._1)
}