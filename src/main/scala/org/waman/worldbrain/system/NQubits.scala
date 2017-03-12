package org.waman.worldbrain.system

import spire.algebra.Trig
import spire.implicits._
import spire.math.Fractional
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

      val p = this.state.map(i)((c, d) => (a_ * c + b_ * d).abs).map(x => x*x).reduce(_+_)

      if(rng.nextDouble() <= p){
        this.state = this.state.updateQubitState(i, basis.first)
        basis.first
      }else{
        this.state = this.state.updateQubitState(i, basis.second)
        basis.second
      }
    }

  /** Observe i-th qubit in standard basis */
  def observeInStandardBasis(i: Int)(implicit ss: StateSpace[A], rng: Generator): StateVector[A] =
    synchronized{
      val p = this.state.map(i)((c, d) => c.abs).map(x => x*x).reduce(_+_)
      if(rng.nextDouble() <= p){
        this.state = this.state.updateQubitStateZero(i)
        ss.zero
      }else{
        this.state = this.state.updateQubitStateOne(i)
        ss.one
      }
    }

  def observeInHadamardBasis(i: Int)(implicit ss: StateSpace[A], rng: Generator): StateVector[A] =
    synchronized{
      val p = this.state.map(i)((c, d) => (c+d).abs).map(x => x*x/2).reduce(_+_)

      if(rng.nextDouble() <= p){
        this.state = this.state.updateQubitStatePlus(i)
        ss.plus
      }else{
        this.state = this.state.updateQubitStateMinus(i)
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