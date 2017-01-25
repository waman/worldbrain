package org.waman.worldbrain.qkd.bb84

import org.waman.worldbrain.Protocol
import org.waman.worldbrain.Protocol._
import org.waman.worldbrain.system.single.Qubit

//***** Message Types *****
case object RequestCorrectBases extends Request
case class QubitMessage(qubits: Seq[Qubit]) extends Protocol.QubitMessage(qubits)
case class CorrectBasisMessage(bases: Seq[Int]) extends ClassicalBitMessage(bases)
case class BasisFilterMessage(filter: Seq[Int]) extends ClassicalBitMessage(filter)
