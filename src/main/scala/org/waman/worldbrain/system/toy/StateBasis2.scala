package org.waman.worldbrain.system.toy

sealed abstract class StateBasis2(val states: Seq[StateVector2])

object StateBasis2{
  import StateVector2._

  case object Standard2 extends StateBasis2(Seq(N00, N01, N10, N11))
  case object Bell extends StateBasis2(Seq(PhiPlus, PhiMinus, PsiPlus, PsiMinus))
}