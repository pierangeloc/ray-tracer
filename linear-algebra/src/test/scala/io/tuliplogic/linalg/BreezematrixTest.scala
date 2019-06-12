package io.tuliplogic.linalg

import org.scalatest.{Inspectors, Matchers, WordSpec}
import breezematrix.Mat
import cats.implicits._

class BreezematrixTest extends WordSpec with Inspectors with Matchers with cats.tests.StrictCatsEquality {
  val convertToEqualizer = ()  // shadow ScalaTest
  val syntax: MatrixOps[breezematrix.Mat, Double] = matrixSyntax[breezematrix.Mat, Double]
  import syntax._

  "breeze based matrix algebra" should {
        import breezematrix.{doubleMatrixAlgebra => alg}

    "initialize correctly a matrix with zeroes" in {
      val zeroes = alg.zero[3, 4]
      forAll(alg.values(zeroes))(_ == 0)
    }

    "initialize correctly a matrix with ones" in {
      val ones = alg.ones[3, 4]
      forAll(alg.values(ones))(_ == 1)
    }

    "update a matrix cell" in {
      val ones = alg.ones[2, 2]
      val expected = alg.create[2, 2](1, 1, 1, 10)
      val res = ones.update(1, 1)(10)
      res =!= expected shouldEqual false
    }

    "add 2 matrices" in {
      val m1 = alg.create[2, 2](
        1, 2,
        3, 4
      )
      val m2 = alg.create[2, 2](
        5, 6,
        7, 8
      )
      m1 + m2  =!= alg.create[2, 2](
        6, 8,
        10, 12
      ) shouldEqual false
    }

    "negate a matrix" in {
      val m = alg.ones[3, 4]
      forAll(alg.values(alg.negate(m)))(_ == -1)
    }

    "transpose a matrix" in {
      val m = alg.create[2, 3](
        1, 2, 3,
        4, 5, 6
      )

      alg.transpose(m) =!= alg.create[3, 2](
        1, 4,
        2, 5,
        3, 6) shouldEqual false
    }

    "multiply matrix by a scalar" in {
      val m = alg.ones[3, 4]
      forAll((m * 10).values)(_ == 10)
    }

    "multiply correctly two matrices" in {
      val m1: Mat[Double, 2, 3] = alg.create[2, 3](
        1, 2, 3,
        4, 5, 6
      )
      val m2 = alg.create[3, 4](
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, 12
      )
      val res = m1 * m2
      val expected = alg.create[2, 4](
        38, 44, 50, 56,
        83, 98, 113, 128
      )
      res =!= expected shouldEqual false
    }

    "compute scalar product" in {
      val v = alg.createCol[4](1, 2, 3, 4)
      val w = alg.createCol[4](5, 6, 7, 8)
      v dot w shouldEqual 70
    }

    "compute cross product" in {
      val v = alg.createCol[3](1, 2, 3)
      val w = alg.createCol[3](2, 3, 4)
      (v cross w)  =!= alg.createCol[3](-1, 2, -1) shouldEqual false
      (w cross v) =!= alg.createCol[3](1, -2, 1) shouldEqual false
    }

  }
}
