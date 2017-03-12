package org.waman.worldbrain.qkd.b92

import spire.math._
import org.waman.worldbrain.Protocol
import org.waman.worldbrain.Protocol.ClassicalBitMessage
import org.waman.worldbrain.system.Qubit

case class QubitMessage[A: Fractional](qubits: Seq[Qubit[A]])
  extends Protocol.QubitMessage(qubits)

case class BitFilterMessage(filter: Seq[Int]) extends ClassicalBitMessage(filter)
