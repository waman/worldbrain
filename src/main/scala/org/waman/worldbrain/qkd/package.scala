package org.waman.worldbrain

package object qkd {

  def applyFilter[E](seq: Seq[E], filter: Seq[Int]): Seq[E] =
    (seq zip filter).filter(_._2 == 1).map(_._1)
}
