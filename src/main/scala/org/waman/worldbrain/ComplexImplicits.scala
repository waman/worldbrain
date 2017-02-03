package org.waman.worldbrain

import spire.algebra._
import spire.math.{Complex, ComplexInstances}

import scala.language.implicitConversions

object ComplexImplicits extends ComplexInstances{

  implicit def convertToComplex[T](real: T)(implicit ev: Semiring[T]): Complex[T] =
    Complex.apply(real)(ev)

  implicit class WorldbrainComplex[T](c: Complex[T])(implicit rg: Ring[T]){
    def unary_~ : Complex[T] = c.conjugate
  }
}
