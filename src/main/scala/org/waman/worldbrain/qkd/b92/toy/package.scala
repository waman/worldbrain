package org.waman.worldbrain.qkd.b92

import org.waman.worldbrain.qkd.applyFilter
import org.waman.worldbrain.system.single.toy.StateBasis
import org.waman.worldbrain.system.single.toy.StateBasis._
import spire.random.Generator

package object toy {

  def createRandomBases(rng: Generator, n: Int): Seq[StateBasis] =
    (0 until n).map{ _ =>
      if(rng.nextBoolean) Standard
      else                Hadamard
    }

  def createRandomBitString(n: Int, rng: Generator): Seq[Int] =
    (0 until n).map(_ => if(rng.nextBoolean) 1 else 0)

  def extractKey(bitString: Seq[Int], filter: Seq[Int]): Seq[Int] =
    applyFilter(bitString, filter)
}
