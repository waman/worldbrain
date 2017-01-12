package org.waman.worldbrain.single

import spire.random.Generator

package object b92 {

  type BitString = Seq[Int]
  type BitFilter = Seq[Int]
  type Key = Seq[Int]

  private[b92] def createRandomBitString(n: Int, rng: Generator): BitString =
    (0 until n).map(_ => if(rng.nextBoolean) 1 else 0)

  private[b92] def extractKey(seq: BitString, filter: BitFilter): Key =
    (seq zip filter).filter(_._2 == 1).map(_._1)
}
