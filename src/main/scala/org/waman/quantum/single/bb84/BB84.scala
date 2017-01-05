package org.waman.quantum.single.bb84

import akka.actor.ActorRef
import org.waman.quantum.Random.nextBoolean
import org.waman.quantum._
import org.waman.quantum.single.BasisVector._
import org.waman.quantum.single.{BasisVector, Qubit, StateBasis}

object BB84{

  //***** Message Types *****
  final case class EstablishKey(bob: ActorRef, n: Int)
  final case class QubitMessage(qubits: Seq[Qubit]) extends OnQuantumChannel
  final case object RequestCorrectBases
  final case class CorrectBasisMessage(bases: Seq[StateBasis])
  final case class BasisFilterMessage(filter: Seq[Boolean])

  def createRandomBases(n: Int): Seq[StateBasis] =
    (0 until n).map{ i =>
      if(nextBoolean) Standard
      else            Hadamard
    }

  def extractKey(stateSeq: Seq[BasisVector], filter: Seq[Boolean]): Seq[Int] =
    applyFilter(stateSeq, filter).map(toBit)

  private def applyFilter(stateSeq: Seq[BasisVector], filter: Seq[Boolean]): Seq[BasisVector] =
    stateSeq.zip(filter).filter(_._2).map(_._1)

  def toBit(state: BasisVector): Int =
    state match {
      case Zero | Plus  => 0
      case One  | Minus => 1
    }
}
