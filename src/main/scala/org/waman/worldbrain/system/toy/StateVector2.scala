package org.waman.worldbrain.system.toy

import spire.implicits._

sealed abstract class StateVector2(val symbol: String){

  def qubitCount = 2
  def dim: Int = 2**qubitCount
  override def toString: String = s"|$symbol>"
}

object StateVector2{

  case object N00 extends StateVector2("00")
  case object N01 extends StateVector2("01")
  case object N10 extends StateVector2("10")
  case object N11 extends StateVector2("11")

  case object PhiPlus  extends StateVector2("Φ+")
  case object PhiMinus extends StateVector2("Φ-")
  case object PsiPlus  extends StateVector2("Ψ+")
  case object PsiMinus extends StateVector2("Ψ-")
}
