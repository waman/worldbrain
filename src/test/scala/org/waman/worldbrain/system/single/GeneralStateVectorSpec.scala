package org.waman.worldbrain.system.single

import org.waman.worldbrain.WorldbrainCustomSpec
import spire.implicits._

class GeneralStateVectorSpec extends WorldbrainCustomSpec{

  "eqv operator should" - {

    "evaluate vector equality" in {
      __SetUp__
      val v = GeneralStateVector(10.0, 20.0)
      val w = GeneralStateVector(10.0, 20.0)
      __Exercise__
      val sut = v eqv w
      __Verify__
      (v eq w) should equal (false)
      sut should equal (true)
    }
  }

  "* operator should" - {

    "calculate inner product" in {
      __SetUp__
      val v = GeneralStateVector(1, 2)
      val w = GeneralStateVector(3, 4)
      __Exercise__
      val sut = v * w
      __Verify__
      sut should equal (11)
    }
  }
}
