package org.waman.worldbrain

import spire.algebra._
import spire.math.Complex

import scala.language.implicitConversions

object ComplexImplicits {

  implicit def convertToComplex[T](real: T)(implicit ev: Semiring[T]): Complex[T] =
    Complex.apply(real)(ev)
}
