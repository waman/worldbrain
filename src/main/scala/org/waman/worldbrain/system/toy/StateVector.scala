package org.waman.worldbrain.system.toy

sealed abstract class StateVector(val symbol: String){

  override def toString: String = s"|$symbol>"
}

object StateVector{

  case object Zero  extends StateVector("0")
  case object One   extends StateVector("1")
  case object Plus  extends StateVector("+")
  case object Minus extends StateVector("-")
}
