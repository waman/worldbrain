package org.waman.quantum.single

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.waman.quantum.single.BasisVector._
import org.waman.quantum.ComplexImplicits._
import spire.math.Complex.i
import spire.implicits._

class BasisVectorSpec extends FreeSpec with TableDrivenPropertyChecks with Matchers{

  val sqrt2 = Math.sqrt(2)

  "probability method should" -{

    "calculate transition probability to the specifed state" in {

      val conversions = Table(
        ("v0", "v1", "expected"),
        (Zero, Zero, 1.0),
        (Zero, One, 0.0),
        (Zero, Plus, 0.5),
        (Zero, Minus, 0.5),
        (Zero, PlusImaginary, 0.5),
        (Zero, MinusImaginary, 0.5),

        (Plus, Plus, 1.0),
        (Plus, Minus, 0.0),
        (Plus, MinusImaginary, 0.5),

        (PlusImaginary, PlusImaginary, 1.0),
        (PlusImaginary, MinusImaginary, 0.0)
      )

      forAll(conversions) { (v0: BasisVector, v1: BasisVector, expected: Double) =>
        val sut = v0 probability v1
        sut should equal(expected +- 0.001)
      }
    }
  }

  "canonical method should" - {

    "change global phase of this BasisVector so that a (coefficient of |0>) be real" in {
      val conversions = Table(
        ("v", "expected"),
        (BasisVector(1, 0), BasisVector(1, 0)),
        (BasisVector(-1, 0), BasisVector(1, 0)),
        (BasisVector(i, 0), BasisVector(1, 0)),

        (BasisVector(0, 1), BasisVector(0, 1)),
        (BasisVector(0, -1), BasisVector(0, 1)),
        (BasisVector(0, i), BasisVector(0, 1)),

        (BasisVector(i/sqrt2, 1/sqrt2), BasisVector(1/sqrt2, -i/sqrt2))
      )

      forAll(conversions){ (v: BasisVector, expected: BasisVector) =>
        val sut = v.canonical
        sut.a should equal (expected.a +- 0.0001)
        sut.b should equal (expected.b +- 0.0001)
      }
    }
  }

  "toAnglesOfBlochSphere method should" - {

    "calculate angles (theta, phi) of Bloch sphere" in {
      val conversions = Table(
        ("v", "expected"),
        (Zero, (0.0, 0.0)),
        (One, (Math.PI, 0.0)),
        (Plus, (Math.PI/2, 0.0)),
        (Minus, (Math.PI/2, Math.PI)),
        (PlusImaginary, (Math.PI/2, Math.PI/2)),
        (MinusImaginary, (Math.PI/2, Math.PI*3/2))
      )

      forAll(conversions){ (v: BasisVector, expected: (Double, Double)) =>
        val sut = v.toAnglesOfBlochSphere
        sut._1 should equal (expected._1 +- 0.001)
        sut._2 should equal (expected._2 +- 0.001)
      }
    }
  }
}
