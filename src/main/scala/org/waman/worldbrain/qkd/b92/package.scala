package org.waman.worldbrain.qkd

import org.waman.worldbrain.qkd.KeyContainer._
import spire.random.Generator

package object b92 {

  private[b92] def createRandomBitString(n: Int, rng: Generator): Seq[Int] =
    (0 until n).map(_ => if(rng.nextBoolean) 1 else 0)

  private[b92] def extractKey(bitString: Seq[Int], filter: Seq[Int]): Seq[Int] =
    applyFilter(bitString, filter)
}
