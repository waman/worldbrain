package org.waman.worldbrain.qkd

import akka.actor.{Actor, ActorRef}
import org.waman.worldbrain.system.single.{StateBasis, StateSpace}
import spire.math.Fractional
import spire.random.Generator

trait AliceFactory{

  def apply[A](keyLength: Int,
               bases: Seq[StateBasis[A]] = Nil,
               nChunk: Int = -1,
               rng: Generator = Generator.rng)
              (implicit a: Fractional[A], ss: StateSpace[A]): Actor = {
    val n = if (nChunk > 0) nChunk else keyLength
    val b: Seq[StateBasis[A]] = if (bases.isEmpty) Seq(ss.standard, ss.hadamard) else bases
    newAlice(keyLength, b, n, rng, a)
  }

  protected def newAlice[A](keyLength: Int, bases: Seq[StateBasis[A]], nChunk: Int,
                            rng: Generator, a: Fractional[A]): Actor
}

trait BobFactory{

  def apply[B](keyLength: Int,
               bases: Seq[StateBasis[B]] = Nil,
               rng: Generator = Generator.rng)
              (implicit a: Fractional[B], ss: StateSpace[B]): Actor = {
    val b: Seq[StateBasis[B]] = if (bases.isEmpty) Seq(ss.standard, ss.hadamard) else bases
    newBob(keyLength, b, rng, a)
  }

  protected def newBob[A](keyLength: Int, bases: Seq[StateBasis[A]],
                          rng: Generator, a: Fractional[A]): Actor
}

trait EveFactory{

  def apply[A](alice: ActorRef,
               bob: ActorRef,
               keyLength: Int,
               bases: Seq[StateBasis[A]] = Nil,
               rng: Generator = Generator.rng)
              (implicit a: Fractional[A], ss: StateSpace[A]): Actor = {
    val b: Seq[StateBasis[A]] = if (bases.isEmpty) Seq(ss.standard, ss.hadamard) else bases
    newEve(alice, bob, keyLength, b, rng, a)
  }

  protected def newEve[A](alice: ActorRef, bob: ActorRef,
                          keyLength: Int, bases: Seq[StateBasis[A]],
                          rng: Generator, a: Fractional[A]): Actor
}


trait FixedBasesAliceFactory[A <: Actor]{

  def apply(keyLength: Int, nChunk: Int = 0, rng: Generator = Generator.rng): A = {
    val n = if(nChunk > 0) nChunk else keyLength
    newAlice(keyLength, n, rng)
  }

  protected def newAlice(keyLength: Int, nChunk: Int, rng: Generator): A
}

trait FixedBasesBobFactory[B <: Actor]{

  def apply(keyLength: Int, rng: Generator = Generator.rng): B =
    newBob(keyLength, rng)

  protected def newBob(keyLength: Int, rng: Generator): B
}

trait FixedBasesEveFactory[E <: Actor]{

  def apply(alice: ActorRef, bob: ActorRef,
            keyLength: Int, rng: Generator = Generator.rng): E =
    newEve(alice, bob, keyLength, rng)

  protected def newEve(alice: ActorRef, bob: ActorRef, keyLength: Int, rng: Generator): E
}
