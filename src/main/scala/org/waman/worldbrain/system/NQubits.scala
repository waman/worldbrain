package org.waman.worldbrain.system

import spire.algebra.Trig
import spire.implicits._
import spire.math._
import spire.random.Generator

import scala.annotation.tailrec

class NQubits[A: Fractional: Trig] private (protected var state: NStateVector[A]){

  def qubitCount: Int = state.qubitCount

  def observe(basis: NStateBasis[A])(implicit rng: Generator): NStateVector[A] =
    synchronized{
      if(basis.contains(this.state)){
        this.state
      }else{
        @tailrec
        def recur(states: Seq[NStateVector[A]], p: A): NStateVector[A] = {
          val trialState = states.head
          val q = p - (state probability trialState)
          if(q <= 0) trialState
          else       recur(states.tail, q)
        }

        val p = implicitly[Fractional[A]].fromDouble(rng.nextDouble)
        val newState = recur(basis.states, p)
        this.state = newState
        newState
      }
    }

  //********** Single Qubit Observation **********
  def observe(i: Int, basis: StateBasis[A])
             (implicit rng: Generator): StateVector[A] =
    synchronized{
      val first = basis.first
      val (a_, b_) = (first.a.conjugate, first.b.conjugate)

      val p = this.state.probabilityOfSingleQubitObservation(i) { (c0, c1) =>
        (a_ * c0 + b_ * c1).abs
      }

      if(rng.nextDouble <= p){
        this.state = this.state.updated(i, basis.first, 1/sqrt(p))
        basis.first
      }else{
        this.state = this.state.updated(i, basis.second, 1/sqrt(1-p))
        basis.second
      }
    }

  /** Observe i-th qubit in standard basis */
  def observeInStandardBasis(i: Int)(implicit ss: StateSpace[A], rng: Generator): StateVector[A] =
    synchronized{
      val p = this.state.probabilityOfSingleQubitObservation(i) { (c0, c1) => c0.abs }

      if(rng.nextDouble <= p){
        this.state = this.state.updatedToZero(i, 1/sqrt(p))
        ss.zero
      }else{
        this.state = this.state.updatedToOne(i, 1/sqrt(1-p))
        ss.one
      }
    }

  def observeInHadamardBasis(i: Int)(implicit ss: StateSpace[A], rng: Generator): StateVector[A] =
    synchronized{
      val p = this.state.probabilityOfSingleQubitObservation(i) { (c0, c1) =>
        (c0 + c1).abs / 2
      }

      if(rng.nextDouble <= p){
        this.state = this.state.updatedToPlus(i, 1/sqrt(p))
        ss.plus
      }else{
        this.state = this.state.updatedToMinus(i, 1/sqrt(1-p))
        ss.minus
      }
    }

  def apply(i: Int) = new Qubit[A]{

    override def observe(basis: StateBasis[A])(implicit rng: Generator) =
      NQubits.this.observe(i, basis)(rng)
  }
}

object NQubits{

  def apply[A: Fractional: Trig](state: NStateVector[A]): NQubits[A] =
    new NQubits(state)
}