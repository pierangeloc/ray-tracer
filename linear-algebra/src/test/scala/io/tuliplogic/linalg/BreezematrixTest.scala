package io.tuliplogic.linalg

import org.scalatest.WordSpec


class BreezematrixTest extends WordSpec {

  "breeze based matrix algebra" should {
    import breezematrix.{doubleMatrixAlgebra => alg}
    import breezematrix.syntax._

    "initialize correctly a matrix with zeroes" in {
      val zeroes = alg.zero[3, 4]
      println(zeroes)
    }

    "multiply correctly two matrices" in {
      val m1 = alg.create[2, 3](1, 2, 3, 4, 5, 6)
      val m2 = alg.create[3, 4](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
      val res = alg.times(m1, m2)
      println(res)
    }
  }
}
