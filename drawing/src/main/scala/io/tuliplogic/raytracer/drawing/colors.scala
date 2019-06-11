package io.tuliplogic.raytracer.drawing

import eu.timepit.refined.{W, refineV}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.{And, Not}
import eu.timepit.refined.numeric.{Greater, Less}
import cats.implicits._
import io.tuliplogic.linalg.breezematrix


object colors {

  import breezematrix.{doubleMatrixAlgebra => alg}
  import alg._
//  import breezematrix.syntax._

  case class RGB(red: Double , green: Double, blue: Double) {
    def toVector: ColVector[3] = alg.createCol[3](red, green, blue)
  }
  object RGB {
    def fromVector(v: ColVector[3]): RGB = RGB(alg.get(v)(0, 0), alg.get(v)(1, 0), alg.get(v)(2, 0))
  }

  type ZeroToOne = Not[Less[W.`0.0`.T]] And Not[Greater[W.`1.0`.T]]
  type Normalized = Double Refined ZeroToOne

  case class NormalizedRGB(redN: Normalized , greenN: Normalized, blueN: Normalized)

  object NormalizedRGB {
    def fromRGB(rgb: RGB): Either[String, NormalizedRGB] = {
      val max = List(rgb.red, rgb.green, rgb.blue).max
      for {
        m <- max.asRight[String].ensure("all r, g, b are zero, can't normalize")(_ != 0d)
        r <- refineV[ZeroToOne](rgb.red / max)
        g <- refineV[ZeroToOne](rgb.green / max)
        b <- refineV[ZeroToOne](rgb.blue / max)
      } yield NormalizedRGB(r, g, b)
    }
  }

}
