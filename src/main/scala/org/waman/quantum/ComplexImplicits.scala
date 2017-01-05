package org.waman.quantum

import scala.language.implicitConversions

import spire.algebra.{Field, Semiring, Trig}
import spire.math.Complex

object ComplexImplicits {

  implicit def convertToComplex[T](real: T)(implicit ev: Semiring[T]): Complex[T] =
    Complex.apply(real)(ev)

  def exp[T](arg: Complex[T])(implicit f: Field[T], t: Trig[T]): Complex[T] = arg.exp
}
