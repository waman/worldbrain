package org.waman.worldbrain.single.b92

import org.waman.worldbrain.single.Protocol.ClassicalBitMessage
import org.waman.worldbrain.single.{Protocol, Qubit}

case class QubitMessage(qubits: Seq[Qubit]) extends Protocol.QubitMessage(qubits)
case class BitFilterMessage(filter: Seq[Int]) extends ClassicalBitMessage(filter)
