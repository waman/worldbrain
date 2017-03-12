package org.waman.worldbrain.system

import spire.algebra.Trig
import spire.math._
import spire.implicits._

import scala.collection.immutable.SortedMap

abstract class NStateVector[A: Fractional: Trig]{ lhs =>

  def qubitCount: Int
  def dim: Int

  def apply(i: Int): Complex[A]
  def coefficients: Seq[Complex[A]]

  def nonZeroCoefficientMap: SortedMap[Int, Complex[A]]

  def *(rhs: NStateVector[A]): Complex[A]

  def probability(rhs: NStateVector[A]): A = {
    val amplitude = (lhs * rhs).abs
    amplitude * amplitude
  }

  //********** For Single Qubit Observation **********
  private def n = qubitCount

  private def forQubits[E](i: Int)(gen: (Int, Int) => E): Seq[E] =
    for(k <- 0 until (2 << i); j <- 0 until (2 << (n-i-1)))
      yield gen(k, j)

  private def f(i: Int): Int => Int = {
    val b = 2 << (n-i)  // = 2**(n-i)
    k => b*k
  }

  private def g(i: Int): Int => Int = {
    val b = 2 << (n-i-1)  // = 2**(n-i-1)
    k => 2 * k + 1
  }

  def map(i: Int)(f: (Complex[A], Complex[A]) => A): Iterable[A] = ???

  def updateQubitState(i: Int, state: StateVector[A]): NStateVector[A] = ???
  def updateQubitStateZero(i: Int): NStateVector[A] = ???
  def updateQubitStateOne(i: Int): NStateVector[A] = ???
  def updateQubitStatePlus(i: Int): NStateVector[A] = ???
  def updateQubitStateMinus(i: Int): NStateVector[A] = ???

  override def toString: String =
    nonZeroCoefficientMap.map{ case (i, c) => s"$c|$i>" }.mkString(" + ")
}

object NStateVector{

  //********** Implementation classes **********
  private class BasisNStateVector[A: Fractional: Trig](val qubitCount: Int, nonZeroIndex: Int)
      extends NStateVector[A]{ lhs =>

    private val delta: PartialFunction[Int, Complex[A]] = {
      case i if i == nonZeroIndex => Complex.one
      case _                      => Complex.zero
    }

    lazy val dim: Int = 1 << qubitCount
    override def apply(i: Int) = delta(i)
    override def coefficients = Seq.tabulate(dim)(delta)
    override def nonZeroCoefficientMap = SortedMap(nonZeroIndex -> Complex.one)
    override def *(rhs: NStateVector[A]) = rhs(nonZeroIndex)
  }

  private class SeqNStateVector[A: Fractional: Trig]
      (val qubitCount: Int, val coefficients: Seq[Complex[A]])
      extends NStateVector[A]{ lhs =>

    override def dim: Int = coefficients.length

    override def apply(i: Int) = coefficients(i)

    override def nonZeroCoefficientMap =
      coefficients.zipWithIndex.filter(_._1 != 0.0).map(e => (e._2, e._1)) ++: SortedMap()

    override def *(rhs: NStateVector[A]): Complex[A] = {
      require(lhs.qubitCount == rhs.qubitCount, "lhs.qubitCount != rhs.qubitCount in * operation")
      (lhs.coefficients zip rhs.coefficients).map {
        case (x, y) => x.conjugate * y
      }.reduce(_+_)
    }
  }

  private class MapNStateVector[A: Fractional: Trig]
      (val qubitCount: Int, cs: SortedMap[Int, Complex[A]])
      extends NStateVector[A]{ lhs =>

    lazy val dim: Int = 1 << qubitCount

    override def apply(i: Int) =
      cs.getOrElse(i, {
        if(0 <= i && i < dim)
          Complex.zero[A]
        else
          throw new IllegalArgumentException(s"The index must be in [0, $dim): $i")
      })

    override def coefficients = (0 until dim).map(apply)

    override def nonZeroCoefficientMap = cs

    override def *(rhs: NStateVector[A]): Complex[A] = {
      require(lhs.qubitCount == rhs.qubitCount, "lhs.qubitCount != rhs.qubitCount in * operation")

      val lhsMap = lhs.nonZeroCoefficientMap
      val rhsMap = rhs.nonZeroCoefficientMap

      val indices = lhsMap.keySet intersect rhsMap.keySet
      indices.map(i => lhsMap(i).conjugate * rhsMap(i)).reduce(_+_)
    }
  }

  //********** factory methods **********
  private[system] val seqMax = 5

  def norm[A: Fractional](cs: Iterable[Complex[A]]): A =
    sqrt(cs.map(c => (c.conjugate * c).abs).reduce(_+_))

  def basisVector[A: Fractional: Trig](qubitCount: Int, i: Int): NStateVector[A] =
    new BasisNStateVector(qubitCount, i)

  private def normalize[A: Fractional](cs: Seq[Complex[A]]): Seq[Complex[A]] = {
    val nrm = norm(cs)
    cs.map(_ / nrm)
  }

  private def normalize[A: Fractional](cs: Map[Int, Complex[A]]): Map[Int, Complex[A]] = {
    val nrm = norm(cs.values)
    cs.mapValues(_ / nrm)
  }

  private def normalize[A: Fractional](cs: SortedMap[Int, Complex[A]]): SortedMap[Int, Complex[A]] = {
    val nrm = norm(cs.values)
    cs.mapValues(_ / nrm)
  }

  def apply[A: Fractional: Trig](qubitCount: Int)(coefficients: Complex[A]*): NStateVector[A] =
    create(qubitCount, coefficients.toList)

  def of[A: Fractional: Trig](qubitCount: Int, coefficients: Seq[Complex[A]]): NStateVector[A] =
    create(qubitCount, coefficients)

  def ofReal[A: Fractional: Trig](qubitCount: Int, coefficients: Seq[A]): NStateVector[A] =
    create(qubitCount, coefficients.map(Complex(_)))

  private def create[A: Fractional: Trig](qubitCount: Int,
                                          coefficients: Seq[Complex[A]]): NStateVector[A] = {
    // For one nonzero component
    val nonZeros = coefficients.zipWithIndex.filter(_._1 != 0)
    if(nonZeros.length == 1)
      return basisVector(qubitCount, nonZeros.head._2)

    qubitCount match {
      case n if 2 <= n && n <= seqMax =>
        val dim = 1 << qubitCount
        require(coefficients.length <= dim,
          s"A number of coefficients must be less that 2**qubitCount: ${coefficients.length}")
        val cs = coefficients.toList.padTo(1 << qubitCount, Complex.zero)
        new SeqNStateVector[A](qubitCount, normalize(cs))

      case n if n > seqMax =>
        val cs = SortedMap(coefficients.zipWithIndex.map(e => (e._2, e._1)).filter(_._2 != 0): _*)
        new MapNStateVector[A](qubitCount, normalize(cs))
    }
  }

  def of[A: Fractional: Trig](qubitCount: Int, coefficients: Map[Int, Complex[A]]): NStateVector[A] =
    create(qubitCount, coefficients)

  def ofReal[A: Fractional: Trig](qubitCount: Int, coefficients: Map[Int, A]): NStateVector[A] =
    create(qubitCount, coefficients.mapValues(Complex(_)))

  private def create[A: Fractional: Trig](qubitCount: Int,
                                          coefficients: Map[Int, Complex[A]]): NStateVector[A] = {
    // For only one nonzero component
    if(coefficients.size == 1)
      return basisVector(qubitCount, coefficients.keys.head)

    qubitCount match {
      case n if 2 <= n && n <= seqMax =>
        val csMap = normalize(coefficients)
        val cs = (0 until (1 << qubitCount)).map(csMap.getOrElse(_, Complex.zero[A]))
        new SeqNStateVector[A](qubitCount, cs)

      case n if n > seqMax =>
        val sorted = coefficients ++: SortedMap[Int, Complex[A]]()
        new MapNStateVector[A](qubitCount, normalize(sorted))
    }
  }
}