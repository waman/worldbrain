package org.waman.worldbrain.system.single

import org.waman.worldbrain.WorldbrainCustomSpec
import org.waman.worldbrain.system.Tolerance
import org.waman.worldbrain.system.single.StateVector._
import spire.implicits._
import spire.math.Complex
import spire.math.Complex.i

class BasisVectorSpec extends WorldbrainCustomSpec{

  val error: Double = 0.000000001
  implicit val tolerance: Tolerance = Tolerance(error)
  val sqrt2: Complex[Double] = Complex(Math.sqrt(2))

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

      forAll(conversions) { (v0: StateVector, v1: StateVector, expected: Double) =>
        __Exercise__
        val sut = v0 probability v1
        __Verify__
        sut should equal(expected +- error)
      }
    }
  }

  "Type conversion methods" - {

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

        forAll(conversions){ (v: StateVector, expected: (Double, Double)) =>
          val sut = v.toAnglesOfBlochSphere
          sut._1 should equal (expected._1 +- error)
          sut._2 should equal (expected._2 +- error)
        }
      }
    }

    "toPointOnBlochSphere method should" - {

      "calculate a coordinate (x, y, z) of Bloch sphere" in {
        val conversions = Table(
          ("v", "expected"),
          (Zero, (0.0, 0.0, 1.0)),
          (One, (0.0, 0.0, -1.0)),
          (Plus, (1.0, 0.0, 0.0)),
          (Minus, (-1.0, 0.0, 0.0)),
          (PlusImaginary, (0.0, 1.0, 0.0)),
          (MinusImaginary, (0.0, -1.0, 0.0))
        )

        forAll(conversions){ (v: StateVector, expected: (Double, Double, Double)) =>
          val sut = v.toPointOnBlochSphere
          sut._1 should equal (expected._1 +- error)
          sut._2 should equal (expected._2 +- error)
          sut._3 should equal (expected._3 +- error)
        }
      }
    }

    "toComplex method should" - {

      "convert a BasisVector to a corresponding Complex number" in {
        val conversions = Table(
          ("v", "expected"),
          (One, Complex(0.0)),
          (Plus, Complex(1.0)),
          (Minus, Complex(-1.0)),
          (PlusImaginary, i[Double]),
          (MinusImaginary, -i[Double])
        )

        forAll(conversions){ (v: StateVector, expected: Complex[Double]) =>
          __Exercise__
          val sut = v.toComplex
          __Verify__
          assert( tolerance.test(sut, expected) )
        }
      }

      "convert |0> to Complex(Double.PositiveInfinity, Double.PositiveInfinity)" in {
        __Exercise__
        val sut = Zero.toComplex
        __Verify__
        sut.real should equal (Double.PositiveInfinity)
        sut.imag should equal (Double.PositiveInfinity)
      }
    }
  }

  "Companion object" - {

    "ofBlochSphere method should" - {

      "create BasisVector object from angle theta on Bloch Sphere with phi = 0" in {
        val conversions = Table(
          ("theta", "expected"),
          (0.0, Zero),
          (Math.PI, One),
          (Math.PI/2, Plus)
        )

        forAll(conversions){ (theta: Double, expected: StateVector) =>
          __Exercise__
          val sut = StateVector.ofBlochSphere(theta)
          __Verify__
          assert( sut hasTheSameCoefficientsAs expected )
        }
      }

      "create BasisVector object from angles (theta, phi) on Bloch Sphere" in {
        val conversions = Table(
          ("theta", "phi", "expected"),
          (0.0, 0.0, Zero),
          (Math.PI, 0.0, One),
          (Math.PI/2, 0.0, Plus),
          (Math.PI/2, Math.PI, Minus),
          (Math.PI/2, Math.PI/2, PlusImaginary),
          (Math.PI/2, 3*Math.PI/2, MinusImaginary)
        )

        forAll(conversions){ (theta: Double, phi: Double, expected: StateVector) =>
          __Exercise__
          val sut = StateVector.ofBlochSphere(theta, phi)
          __Verify__
          assert( sut hasTheSameCoefficientsAs expected )
        }
      }
    }
  }
}
