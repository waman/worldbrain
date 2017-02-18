package org.waman.worldbrain.system.single.toy

import org.waman.worldbrain.system

sealed abstract class StateVector(val symbol: String) extends system.StateVector {

  override def toString: String = s"|$symbol>"
}

object StateVector{

  object Zero  extends StateVector("0")
  object One   extends StateVector("1")
  object Plus  extends StateVector("+")
  object Minus extends StateVector("-")
}
