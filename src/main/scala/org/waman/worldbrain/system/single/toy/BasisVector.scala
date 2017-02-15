package org.waman.worldbrain.system.single.toy

import org.waman.worldbrain.system

sealed abstract class BasisVector(val symbol: String) extends system.BasisVector {

  override def toString: String = s"|$symbol>"
}

object BasisVector{

  object Zero  extends BasisVector("0")
  object One   extends BasisVector("1")
  object Plus  extends BasisVector("+")
  object Minus extends BasisVector("-")
}
