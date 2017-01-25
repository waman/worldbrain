package org.waman.worldbrain.qkd.b92

import org.waman.worldbrain.Protocol
import org.waman.worldbrain.Protocol.ClassicalBitMessage
import org.waman.worldbrain.system.single.Qubit

case class QubitMessage(qubits: Seq[Qubit]) extends Protocol.QubitMessage(qubits)
case class BitFilterMessage(filter: Seq[Int]) extends ClassicalBitMessage(filter)
