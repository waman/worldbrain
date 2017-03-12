package org.waman.worldbrain.qkd.b92

import org.waman.worldbrain.system
import org.waman.worldbrain.qkd.applyFilter
import org.waman.worldbrain.system.StateBasis
import spire.random.Generator

trait StateAlias[A] extends system.StateAlias[A]{

  def createRandomBases(n: Int)(implicit rng: Generator): Seq[StateBasis[A]] =
    (0 until n).map(_ => rng.chooseFromSeq(bases)(rng))

  def createRandomBitString(n: Int)(implicit rng: Generator): Seq[Int] =
    (0 until n).map(_ => if(rng.nextBoolean) 1 else 0)

  def extractKey(bitString: Seq[Int], filter: Seq[Int]): Seq[Int] =
    applyFilter(bitString, filter)

}
