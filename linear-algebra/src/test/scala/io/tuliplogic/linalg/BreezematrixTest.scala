package io.tuliplogic.linalg

import org.scalatest.{Inspectors, Matchers, WordSpec}
import cats.implicits._
import cats.kernel.Eq


class BreezematrixTest extends WordSpec with Inspectors with Matchers {

  "breeze based matrix algebra" should {
    import breezematrix.{doubleMatrixAlgebra => alg}
    import breezematrix.Mat
    import breezematrix.syntax._

    "initialize correctly a matrix with zeroes" in {
      val zeroes = alg.zero[3, 4]
      forAll(alg.values(zeroes))(_ == 0)
    }

    "initialize correctly a matrix with ones" in {
      val ones = alg.ones[3, 4]
      forAll(alg.values(ones))(_ == 1)
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
      val res = alg.times(m1, m2)
      val expected = alg.create[2, 4](
        38, 44, 50, 56,
        83, 98, 113, 128
      )
      println("res: "+ res)
      println("exp: "+ expected)
      println("delta" + alg.values(res).zip(alg.values(expected)).map {
          case (x, y) => math.abs(x - y)
        }.sum
      )
      Eq[Mat[Double, 2, 4]].eqv(res, expected) shouldEqual true
    }
  }
}
