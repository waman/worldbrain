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
  def probabilityOfSingleQubitObservation(i: Int)(f: (Complex[A], Complex[A]) => A): A =
    coefficientPairs(i)
        .map(x => f(x._1, x._2))
        .map(x => x * x)
        .reduce(_ + _)

  protected def coefficientPairs(i: Int): Iterator[(Complex[A], Complex[A])]

  def updated(i: Int, state: StateVector[A]): NStateVector[A]
  /** nf (normalization factor) must be 1/sqrt(p) or 1/sqrt(1-p) */
  def updated(i: Int, state: StateVector[A], nf: A): NStateVector[A]

  def updatedToZero(i: Int)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.zero)
  def updatedToZero(i: Int, nf: A)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.zero, nf)

  def updatedToOne(i: Int)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.one)
  def updatedToOne(i: Int, nf: A)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.one, nf)

  def updatedToPlus(i: Int)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.plus)
  def updatedToPlus(i: Int, nf: A)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.plus, nf)

  def updatedToMinus(i: Int)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.minus)
  def updatedToMinus(i: Int, nf: A)(implicit ss: StateSpace[A]): NStateVector[A] =
    updated(i, ss.minus, nf)


  override def toString: String = {
    def toString(c: Complex[A]): String =
      if(c.isReal) c.real.toString else c.toString

    nonZeroCoefficientMap.map { case (i, c) => s"${toString(c)}|$i>" }.mkString(" + ")
  }
}

object NStateVector{

  //********** Implementation classes **********
  /** |nonZeroIndex>: pure state (in the standard basis) */
  private class BasisNStateVector[A: Fractional: Trig]
      (val qubitCount: Int, nonZeroIndex: Int)
      extends NStateVector[A]{ lhs =>

    private final def n = qubitCount
    private final def cZero = Complex.zero[A]
    private final def cOne  = Complex.one[A]
    private final def s2i   = Complex(1/sqrt(implicitly[Fractional[A]].fromInt(2)))

    private val delta: PartialFunction[Int, Complex[A]] = {
      case i if i == nonZeroIndex => cOne
      case _                      => cZero
    }

    override val dim: Int = 1 << qubitCount
    require(0 <= nonZeroIndex && nonZeroIndex < dim, s"Non-zero index must be in the range [0, $dim)")

    override def apply(i: Int) = delta(i)
    override lazy val coefficients = Seq.tabulate(dim)(delta)
    override def nonZeroCoefficientMap = SortedMap(nonZeroIndex -> cOne)
    override def *(rhs: NStateVector[A]) = rhs(nonZeroIndex)

    def ithStateIsZero(i: Int): Boolean = (nonZeroIndex & (1 << (n-i-1))) == 0

    override protected def coefficientPairs(i: Int): Iterator[(Complex[A], Complex[A])] =
      if(ithStateIsZero(i)) Iterator((cOne , cZero))
      else                  Iterator((cZero, cOne))

    override def updated(i: Int, state: StateVector[A]) = updated(i, state, 1)

    override def updated(i: Int, state: StateVector[A], nf: A) = {
      val (a, b) = (state.a, state.b)

      val mask = 1 << (n-i-1)
      val j = nonZeroIndex ^ mask

      if((nonZeroIndex & mask) == 0){
        val a_ = a.conjugate
        val newCs = coefficients.updated(nonZeroIndex, a*a_).updated(j, b*a_)
        new SeqNStateVector(qubitCount, newCs)
      }else{
        val b_ = b.conjugate
        val newCs = coefficients.updated(j, a*b_).updated(nonZeroIndex, b*b_)
        new SeqNStateVector(qubitCount, newCs)
      }
    }

    override def updatedToZero(i: Int)(implicit ss: StateSpace[A]) = updatedToZero(i, 1)

    override def updatedToZero(i: Int, nf: A)(implicit ss: StateSpace[A]) = {
      require(ithStateIsZero(i))
      this
    }

    override def updatedToOne(i: Int)(implicit ss: StateSpace[A]) = updatedToOne(i, 1)

    override def updatedToOne(i: Int, nf: A)(implicit ss: StateSpace[A]) = {
      require(!ithStateIsZero(i))
      this
    }

    override def updatedToPlus(i: Int)(implicit ss: StateSpace[A]) = updatedToPlus(i, 1)

    override def updatedToPlus(i: Int, nf: A)(implicit ss: StateSpace[A]) = {
      val mask = 1 << (n-i-1)
      val newCs = coefficients.updated(nonZeroIndex, s2i)
                              .updated(nonZeroIndex ^ mask, s2i)
      new SeqNStateVector(qubitCount, newCs)
    }

    override def updatedToMinus(i: Int)(implicit ss: StateSpace[A]) = updatedToMinus(i, 1)

    override def updatedToMinus(i: Int, nf: A)(implicit ss: StateSpace[A]) = {
      val mask = 1 << (n-i-1)
      val (p, q) =
        if((nonZeroIndex & mask) == 0)
          (nonZeroIndex, nonZeroIndex ^ mask)
        else
          (nonZeroIndex ^ mask, nonZeroIndex)

      val newCs = coefficients.updated(p, s2i).updated(q, -s2i)
      new SeqNStateVector(qubitCount, newCs)
    }
  }

  private abstract class SeqNStateVectorAdapter[A: Fractional: Trig]
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

    protected final def n: Int = qubitCount
    protected final def cZero: Complex[A] = Complex.zero[A]
    protected final def cOne : Complex[A] = Complex.one[A]

    private def createNormalizedVector(cs: Seq[Complex[A]]): NStateVector[A] =
      new SeqNStateVector(qubitCount, normalize(cs))

    private def createVector(cs: Seq[Complex[A]]): NStateVector[A] =
      new SeqNStateVector(qubitCount, cs)

    // General basis of a qubit
    override def updated(i: Int, state: StateVector[A]): NStateVector[A] =
      createNormalizedVector(calculateCoefficients(i, state, 1))

    override def updated(i: Int, state: StateVector[A], nf: A): NStateVector[A] =
      createVector(calculateCoefficients(i, state, nf))

    protected def calculateCoefficients(i: Int, state: StateVector[A], nf: A): Seq[Complex[A]]

    // |0>
    override def updatedToZero(i: Int)(implicit ss: StateSpace[A]) =
      createNormalizedVector(calculateCoefficientsZero(i, 1))

    override def updatedToZero(i: Int, nf: A)(implicit ss: StateSpace[A]) =
      createVector(calculateCoefficientsZero(i, nf))

    protected def calculateCoefficientsZero(i: Int, nf: A): Seq[Complex[A]]

    // |1>
    override def updatedToOne(i: Int)(implicit ss: StateSpace[A]) =
      createNormalizedVector(calculateCoefficientsOne(i, 1))

    override def updatedToOne(i: Int, nf: A)(implicit ss: StateSpace[A]) =
      createVector(calculateCoefficientsOne(i, nf))

    protected def calculateCoefficientsOne(i: Int, nf: A): Seq[Complex[A]]

    // |+>
    override def updatedToPlus(i: Int)(implicit ss: StateSpace[A]) =
      createNormalizedVector(calculateCoefficientsPlus(i, 1))

    override def updatedToPlus(i: Int, nf: A)(implicit ss: StateSpace[A]) =
      createVector(calculateCoefficientsPlus(i, nf))

    protected def calculateCoefficientsPlus(i: Int, nf: A): Seq[Complex[A]]

    // |1>
    override def updatedToMinus(i: Int)(implicit ss: StateSpace[A]) =
      createNormalizedVector(calculateCoefficientsMinus(i, 1))

    override def updatedToMinus(i: Int, nf: A)(implicit ss: StateSpace[A]) =
      createVector(calculateCoefficientsMinus(i, nf))

    protected def calculateCoefficientsMinus(i: Int, nf: A): Seq[Complex[A]]
  }

  private class SeqNStateVector2[A: Fractional: Trig]
      (qubitCount: Int, coefficients: Seq[Complex[A]])
      extends SeqNStateVectorAdapter[A](qubitCount, coefficients){

    private final def c0 = coefficients.head
    private final def c1 = coefficients(1)
    private final def c2 = coefficients(2)
    private final def c3 = coefficients(3)

    override protected def coefficientPairs(i: Int): Iterator[(Complex[A], Complex[A])] =
      i match {
        case 0 => Iterator((c0, c2), (c1, c3))
        case 1 => Iterator((c0, c2), (c2, c3))
      }

    override protected def calculateCoefficients(i: Int, state: StateVector[A], nf: A) = {
      val (a, b) = (state.a, state.b)
      val (a_, b_) = (a.conjugate, b.conjugate)

      i match {
        case 0 =>
          val c02 = (a_ * c0 + b_ * c2) * nf
          val c13 = (a_ * c1 + b_ * c3) * nf
          Seq(a * c02, a * c13, b * c02, b * c13)

        case 1 =>
          val c01 = (a_ * c0 + b_ * c1) * nf
          val c23 = (a_ * c2 + b_ * c3) * nf
          Seq(a * c01, a * c23, b * c01, b * c23)
      }
    }

    override protected def calculateCoefficientsZero(i: Int, nf: A) =
      i match {
        case 0 => Seq(c0 * nf, c1 * nf,   cZero, cZero)
        case 1 => Seq(c0 * nf,   cZero, c2 * nf, cZero)
      }

    override protected def calculateCoefficientsOne(i: Int, nf: A) =
      i match {
        case 0 => Seq(cZero,   cZero, c2 * nf, c3 * nf)
        case 1 => Seq(cZero, c1 * nf,   cZero, c3 * nf)
      }

    override protected def calculateCoefficientsPlus(i: Int, nf: A) =
      i match {
        case 0 =>
          val c02 = (c0 + c2) / 2.0 * nf
          val c13 = (c1 + c3) / 2.0 * nf
          Seq(c02, c13, c02, c13)

        case 1 =>
          val c01 = (c0 + c1) / 2.0 * nf
          val c23 = (c2 + c3) / 2.0 * nf
          Seq(c01, c01, c23, c23)
      }

    override protected def calculateCoefficientsMinus(i: Int, nf: A) =
      i match {
        case 0 =>
          val c02 = (c0 - c2) / 2.0 * nf
          val c13 = (c1 - c3) / 2.0 * nf
          Seq(c02, c13, -c02, -c13)

        case 1 =>
          val c01 = (c0 - c1) / 2.0 * nf
          val c23 = (c2 - c3) / 2.0 * nf
          Seq(c01, -c01, c23, -c23)
      }
  }

  private class SeqNStateVector[A: Fractional: Trig]
      (qubitCount: Int, coefficients: Seq[Complex[A]])
      extends SeqNStateVectorAdapter[A](qubitCount, coefficients){

    /**
      * n = 4, i = 0
      * |0  1  2  3  4  5  6  7| =  |0000 0001 0010 0011 ...|
      * |8  9 10 11 12 13 14 15|    |1000 1001 1010 1011 ...|
      *
      * n = 4, i = 2
      * |0  1  2  3| 8  9 10 11| =  |0000 0001 0010 0011|...|
      * |4  5  6  7|12 13 14 15|    |0100 0101 0110 0111|...|
      */
    override protected def coefficientPairs(i: Int): Iterator[(Complex[A], Complex[A])] =
      groupedCoefficientPairs(i).flatten

    private def groupedCoefficientPairs(i: Int): Iterator[Seq[(Complex[A], Complex[A])]] = {
      val gSize = 1 << (n-i)
      coefficients.grouped(gSize).map{ g =>
        val (cs0, cs1) = g.splitAt(gSize/2)
        cs0 zip cs1
      }
    }

    override protected
    def calculateCoefficients(i: Int, state: StateVector[A], nf: A) = {
      val (a, b) = (state.a, state.b)
      val (a_, b_) = (a.conjugate, b.conjugate)

      groupedCoefficientPairs(i).flatMap { g =>
        val ss = g.map { c =>
          Seq(a * (a_ * c._1 + b_ * c._2) * nf, b * (a_ * c._1 + b_ * c._2))
        }.transpose
        ss.head ++: ss(2)
      }.toSeq
    }

    private def zeros(i: Int): Seq[Complex[A]] =
      Seq.fill(1 << (n-i-1))(Complex.zero[A])

    override protected def calculateCoefficientsZero(i: Int, nf: A) = {
      val zs = zeros(i)
      groupedCoefficientPairs(i).flatMap(g => g.map(_._1 * nf) ++: zs).toSeq
    }

    override protected def calculateCoefficientsOne(i: Int, nf: A) = {
      val zs = zeros(i)
      groupedCoefficientPairs(i).flatMap(g => zs ++: g.map(_._2 * nf)).toSeq
    }

    override protected def calculateCoefficientsPlus(i: Int, nf: A) =
      groupedCoefficientPairs(i).flatMap{ g =>
        val s = g.map(c => (c._1 + c._2) / 2.0 * nf)
        s ++: s
      }.toSeq

    override protected def calculateCoefficientsMinus(i: Int, nf: A) =
      groupedCoefficientPairs(i).flatMap{ g =>
        val s = g.map(c => (c._1 - c._2) / 2.0 * nf)
        s ++: s.map(-_)
      }.toSeq
  }

  //********** Private Utilities **********
  def norm[A: Fractional](cs: Iterable[Complex[A]]): A =
    sqrt(cs.map(c => (c.conjugate * c).abs).reduce(_+_))

  private def normalize[A: Fractional](cs: Seq[Complex[A]]): Seq[Complex[A]] = {
    val nrm = norm(cs)
    cs.map(_ / nrm)
  }

  //********** factory methods **********
  def basisVector[A: Fractional: Trig](qubitCount: Int, i: Int): NStateVector[A] =
    new BasisNStateVector(qubitCount, i)

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
    if(nonZeros.length == 1) {
      basisVector(qubitCount, nonZeros.head._2)
    }else{
      val dim = 1 << qubitCount
      require(coefficients.length <= dim,
        s"A number of coefficients must be less that 2**qubitCount: ${coefficients.length}")

      val cs = coefficients.padTo(1 << qubitCount, Complex.zero)
      new SeqNStateVector[A](qubitCount, normalize(cs))
    }
  }

  def of[A: Fractional: Trig](qubitCount: Int, coefficients: Map[Int, Complex[A]]): NStateVector[A] =
    create(qubitCount, coefficients)

  def ofReal[A: Fractional: Trig](qubitCount: Int, coefficients: Map[Int, A]): NStateVector[A] =
    create(qubitCount, coefficients.mapValues(Complex(_)))

  private def create[A: Fractional: Trig](qubitCount: Int,
                                          coefficients: Map[Int, Complex[A]]): NStateVector[A] = {
    // For only one nonzero component
    if(coefficients.size == 1) {
      basisVector(qubitCount, coefficients.keys.head)
    }else{
      val nrm = norm(coefficients.values)
      val normalized = coefficients.mapValues(_ / nrm)
      val cs = (0 until (1 << qubitCount)).map(i => normalized.getOrElse(i, Complex.zero[A]))
      new SeqNStateVector(qubitCount, cs)
    }
  }
}