package org.waman.worldbrain.system.single

import org.waman.worldbrain.WorldbrainCustomSpec
import org.waman.worldbrain.system.Tolerance
import spire.implicits._
import spire.math.Complex
import spire.math.Complex.i

class BasisVectorSpec extends WorldbrainCustomSpec{

  val error: Double = 0.000000001
  implicit val tolerance: Tolerance[Double] = Tolerance(error)
  val sqrt2: Complex[Double] = Complex(Math.sqrt(2))


  "probability method should" -{
    import StateSpace.DoubleStateSpace._

    "calculate transition probability to the specified state" in {

      val conversions = Table(
        ("v0", "v1", "expected"),
        (zero, zero, 1.0),
        (zero, one, 0.0),
        (zero, plus, 0.5),
        (zero, minus, 0.5),
        (zero, iPlus, 0.5),
        (zero, iMinus, 0.5),

        (plus, plus, 1.0),
        (plus, minus, 0.0),
        (plus, iMinus, 0.5),

        (iPlus, iPlus, 1.0),
        (iPlus, iMinus, 0.0)
      )

      forAll(conversions) { (v0: BasisVector[Double], v1: BasisVector[Double], expected: Double) =>
        __Exercise__
        val sut = v0 probability v1
        __Verify__
        sut should equal(expected +- error)
      }
    }

    "generics" in {

    }
  }

  "Type conversion methods" - {
    import StateSpace.DoubleStateSpace._

    "toAnglesOfBlochSphere method should" - {

      "calculate angles (theta, phi) of Bloch sphere" in {
        val conversions = Table(
          ("v", "expected"),
          (zero, (0.0, 0.0)),
          (one, (Math.PI, 0.0)),
          (plus, (Math.PI/2, 0.0)),
          (minus, (Math.PI/2, Math.PI)),
          (iPlus, (Math.PI/2, Math.PI/2)),
          (iMinus, (Math.PI/2, Math.PI*3/2))
        )

        forAll(conversions){ (v: BasisVector[Double], expected: (Double, Double)) =>
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
          (zero, (0.0, 0.0, 1.0)),
          (one, (0.0, 0.0, -1.0)),
          (plus, (1.0, 0.0, 0.0)),
          (minus, (-1.0, 0.0, 0.0)),
          (iPlus, (0.0, 1.0, 0.0)),
          (iMinus, (0.0, -1.0, 0.0))
        )

        forAll(conversions){ (v: BasisVector[Double], expected: (Double, Double, Double)) =>
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
          (one, Complex(0.0)),
          (plus, Complex(1.0)),
          (minus, Complex(-1.0)),
          (iPlus, i[Double]),
          (iMinus, -i[Double])
        )

        forAll(conversions){ (v: BasisVector[Double], expected: Complex[Double]) =>
          __Exercise__
          val sut = v.toComplex
          __Verify__
          assert( tolerance.test(sut, expected) )
        }
      }

//      "convert |0> to Complex(Double.PositiveInfinity, Double.PositiveInfinity)" in {
//        __Exercise__
//        val sut = zero.toComplex
//        __Verify__
//        sut.real should equal (Double.PositiveInfinity)
//        sut.imag should equal (Double.PositiveInfinity)
//      }
    }
  }

  "Companion object" - {
    import StateSpace.DoubleStateSpace._

    "ofBlochSphere method should" - {

      "create BasisVector object from angle theta on Bloch Sphere with phi = 0" in {
        val conversions = Table(
          ("theta", "expected"),
          (0.0, zero),
          (Math.PI, one),
          (Math.PI/2, plus)
        )

        forAll(conversions){ (theta: Double, expected: BasisVector[Double]) =>
          __Exercise__
          val sut = BasisVector.ofBlochSphere(theta)
          __Verify__
          assert( sut hasTheSameCoefficientsAs expected )
        }
      }

      "create BasisVector object from angles (theta, phi) on Bloch Sphere" in {
        val conversions = Table(
          ("theta", "phi", "expected"),
          (0.0, 0.0, zero),
          (Math.PI, 0.0, one),
          (Math.PI/2, 0.0, plus),
          (Math.PI/2, Math.PI, minus),
          (Math.PI/2, Math.PI/2, iPlus),
          (Math.PI/2, 3*Math.PI/2, iMinus)
        )

        forAll(conversions){ (theta: Double, phi: Double, expected: BasisVector[Double]) =>
          __Exercise__
          val sut = BasisVector.ofBlochSphere(theta, phi)
          __Verify__
          assert( sut hasTheSameCoefficientsAs expected )
        }
      }
    }
  }
}
