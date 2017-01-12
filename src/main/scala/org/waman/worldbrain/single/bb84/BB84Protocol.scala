package org.waman.worldbrain.single.bb84

import org.waman.worldbrain.single.Protocol._
import org.waman.worldbrain.single.{Protocol, Qubit}

//***** Message Types *****
case object RequestCorrectBases extends Request
case class QubitMessage(qubits: Seq[Qubit]) extends Protocol.QubitMessage(qubits)
case class CorrectBasisMessage(bases: Seq[Int]) extends ClassicalBitMessage(bases)
case class BasisFilterMessage(filter: Seq[Int]) extends ClassicalBitMessage(filter)
