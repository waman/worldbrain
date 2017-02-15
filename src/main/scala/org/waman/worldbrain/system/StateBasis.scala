package org.waman.worldbrain.system

case class StateBasis[V <: BasisVector](states: Seq[V]){

  require(states.length == 2,
    "Single qubit basis must have only two state vectors: " + states.length)

  def contains(state: V): Boolean = states.contains(state)
  def indexOf(state: V): Int = states.indexOf(state)

  def first : V = states.head
  def second: V = states(1)}