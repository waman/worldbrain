package org.waman.worldbrain.system

import org.waman.worldbrain.WorldbrainCustomSpec
import scala.math._
import spire.implicits._

class NStateVectorSpec extends WorldbrainCustomSpec{

  val s2i: Double = 1/sqrt(2)

  "coefficients method should" - {

    "return Seq of coefficients (constructed by Seq)" in {
      val conversions = Table(
        ("n", "cs", "expected"),
        (3, Seq(0.0, 1.0), Seq.fill(8)(0.0).updated(1, 1.0)),
        (2, Seq(1.0, 0.0, 0.0, 1.0), Seq(s2i, 0, 0, s2i)),
        (2, Seq(1.0, 0.0, 1.0), Seq(s2i, 0, s2i, 0)),
        (10, Seq(1.0, 0.0, 1.0), Seq.fill(1024)(0.0).updated(0, s2i).updated(2, s2i))
      )

      forAll(conversions){ (n: Int, cs: Seq[Double], expected: Seq[Double]) =>
        __SetUp__
        val v = NStateVector.ofReal(n, cs)
        __Exercise__
        val sut = v.coefficients
        __Verify__
        sut should equal (expected)
      }
    }

    "return Seq of coefficients (constructed by Map)" in {
      val conversions = Table(
        ("n", "cs", "expected"),
        (3, Map(1 -> 1.0), Seq.fill(8)(0.0).updated(1, 1.0)),
        (2, Map(0 -> 1.0, 3 -> 1.0), Seq(s2i, 0, 0, s2i)),
        (10, Map(0 -> 1.0, 2 -> 1.0), Seq.fill(1024)(0.0).updated(0, s2i).updated(2, s2i))
      )

      forAll(conversions){ (n: Int, cs: Map[Int, Double], expected: Seq[Double]) =>
        __SetUp__
        val v = NStateVector.ofReal(n, cs)
        __Exercise__
        val sut = v.coefficients
        __Verify__
        sut should equal (expected)
      }
    }
  }

  "nonZeroCoefficients method should" - {

    "return Map of nonzero coefficients (constructed by Seq)" in {
      val conversions = Table(
        ("n", "cs", "expected"),
        (3, Seq(0.0, 1.0), Map(1 -> 1.0)),
        (2, Seq(1.0, 0.0, 0.0, 1.0), Map(0 -> s2i, 3 -> s2i)),
        (2, Seq(1.0, 0.0, 1.0), Map(0 -> s2i, 2 -> s2i)),
        (10, Seq(1.0, 0.0, 1.0), Map(0 -> s2i, 2 -> s2i))
      )

      forAll(conversions){ (n: Int, cs: Seq[Double], expected: Map[Int, Double]) =>
        __SetUp__
        val v = NStateVector.ofReal(n, cs)
        __Exercise__
        val sut = v.nonZeroCoefficientMap
        __Verify__
        sut should equal (expected)
      }
    }

    "return Map of nonzero coefficients (constructed by Map)" in {
      val conversions = Table(
        ("n", "cs", "expected"),
        (3, Map(1 -> 1.0), Map(1 -> 1.0)),
        (2, Map(0 -> 1.0, 3 -> 1.0), Map(0 -> s2i, 3 -> s2i)),
        (10, Map(0 -> 1.0, 2 -> 1.0), Map(0 -> s2i, 2 -> s2i))
      )

      forAll(conversions){ (n: Int, cs: Map[Int, Double], expected: Map[Int, Double]) =>
        __SetUp__
        val v = NStateVector.ofReal(n, cs)
        __Exercise__
        val sut = v.nonZeroCoefficientMap
        __Verify__
        sut should equal (expected)
      }
    }
  }

  "updatedToZero method should" - {

    "return i-th qubit state set to the zero of the standard basis" in {
      import NStateSpace2.doubleStateSpace._

      val conversions = Table(
        ("i", "v", "expected"),
        // Standard states
        // ~ |00>
        (0, zero , Seq(1.0, 0.0, 0.0, 0.0)),
        (1, zero , Seq(1.0, 0.0, 0.0, 0.0)),
        // ~ |01>
        (0, one, Seq(0.0, 1.0, 0.0, 0.0)),
        // ~ |10>
        (1, two , Seq(0.0, 0.0, 1.0, 0.0)),
        // ~ |11>

        // Bell states
        // ~ |00> + |11>
        (0, phiPlus , Seq(1.0, 0.0, 0.0, 0.0)),
        (1, phiPlus , Seq(1.0, 0.0, 0.0, 0.0)),
        // ~ |00> - |11>
        (0, phiMinus, Seq(1.0, 0.0, 0.0, 0.0)),
        (1, phiMinus, Seq(1.0, 0.0, 0.0, 0.0)),
        // ~ |01> + |10>
        (0, psiPlus , Seq(0.0, 1.0, 0.0, 0.0)),
        (1, psiPlus , Seq(0.0, 0.0, 1.0, 0.0)),
          // ~ |01> - |10>
        (0, psiMinus, Seq(0.0, 1.0, 0.0, 0.0)),
        (1, psiMinus, Seq(0.0, 0.0, -1.0, 0.0))
      )

      forAll(conversions){ (i: Int, v: NStateVector[Double], expected: Seq[Double]) =>
        __Exercise__
        val sut = v.updatedToZero(i)
        __Verify__
        sut.coefficients should equal (expected)
      }
    }

    "throw an exception for some states" in {

      import NStateSpace2.doubleStateSpace._

      val conversions = Table(
        ("i", "v"),
        (1, one),
        (0, two),
        (0, three),
        (1, three)
      )

      forAll(conversions){ (i: Int, v: NStateVector[Double]) =>
        __Verify__
        an [Exception] should be thrownBy{
          v.updatedToZero(i)
        }
      }
    }
  }

  "updatedToOne method should" - {

    "return i-th qubit state set to the one of the standard basis" in {
      import NStateSpace2.doubleStateSpace._

      val conversions = Table(
        ("i", "v", "expected"),
        // Standard states
        // ~ |00>
        // ~ |01>
        (1, one, Seq(0.0, 1.0, 0.0, 0.0)),
        // ~ |10>
        (0, two , Seq(0.0, 0.0, 1.0, 0.0)),
        // ~ |11>
        (0, three, Seq(0.0, 0.0, 0.0, 1.0)),
        (1, three, Seq(0.0, 0.0, 0.0, 1.0)),

        // Bell states
        // ~ |00> + |11>
        (0, phiPlus , Seq(0.0, 0.0, 0.0, 1.0)),
        (1, phiPlus , Seq(0.0, 0.0, 0.0, 1.0)),
        // ~ |00> - |11>
        (0, phiMinus, Seq(0.0, 0.0, 0.0, -1.0)),
        (1, phiMinus, Seq(0.0, 0.0, 0.0, -1.0)),
        // ~ |01> + |10>
        (0, psiPlus , Seq(0.0, 0.0, 1.0, 0.0)),
        (1, psiPlus , Seq(0.0, 1.0, 0.0, 0.0)),
        // ~ |01> - |10>
        (0, psiMinus, Seq(0.0, 0.0, -1.0, 0.0)),
        (1, psiMinus, Seq(0.0, 1.0, 0.0, 0.0))
      )

      forAll(conversions){ (i: Int, v: NStateVector[Double], expected: Seq[Double]) =>
        __Exercise__
        val sut = v.updatedToOne(i)
        __Verify__
        sut.coefficients should equal (expected)
      }
    }

    "throw an exception for some states" in {

      import NStateSpace2.doubleStateSpace._

      val conversions = Table(
        ("i", "v"),
        (0, zero),
        (1, zero),
        (0, one),
        (1, two)
      )

      forAll(conversions){ (i: Int, v: NStateVector[Double]) =>
        __Verify__
        an [Exception] should be thrownBy{
          v.updatedToOne(i)
        }
      }
    }
  }

  "companion object" - {

    "apply factory method should" - {

      "work well for vararg" in {
        val phiPlus = NStateVector(2)(1.0, 1.0)
        println(phiPlus)
      }

      "work well for map" in {
        val phiPlus = NStateVector.ofReal(2, Map(0 -> 1.0, 3 -> 1.0))
        println(phiPlus)
      }
    }
  }
}
