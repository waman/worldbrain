package org.waman.worldbrain

import spire.algebra._
import spire.math.Complex

import scala.language.implicitConversions

object ComplexImplicits {

  implicit def convertToComplex[T](real: T)(implicit ev: Semiring[T]): Complex[T] =
    Complex.apply(real)(ev)

//  def sqrt[T](arg: Complex[T])
//             (implicit f: Field[T], n0: NRoot[T], o: IsReal[T]): Complex[T] = arg.sqrt
//
//  def exp[T](arg: Complex[T])(implicit f: Field[T], t: Trig[T]): Complex[T] = arg.exp
}
