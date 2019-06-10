package io.tuliplogic.raytracer.drawing

import io.tuliplogic.linalg.{MinimalMatrix, breezematrix}
import io.tuliplogic.raytracer.drawing.colors.RGB
import singleton.ops.{<, Require}
import io.tuliplogic.linalg.Dim
/**
  *
  * ray-tracer - 2019-06-08
  * Created with ♥ in Amsterdam
  */
object canvas extends MinimalMatrix {
  import breezematrix.{doubleMatrixAlgebra => alg}

  class Canvas[M <: Dim : ValueOf, N <: Dim : ValueOf] private (
    r: breezematrix.Mat[Double, M, N],
    g: breezematrix.Mat[Double, M, N],
    b: breezematrix.Mat[Double, M, N]
  )
  {
    def get[I <: Dim : ValueOf, J <: Dim : ValueOf](i: I, j: J)
      (implicit lteqRow: Require[I < M], lteqCol: Require[J < N]): RGB = {
      RGB(alg.get(r)(i, j), alg.get(g)(i, j), alg.get(b)(i, j))
    }
  }

  object Canvas {
    def create[M <: Dim : ValueOf, N <: Dim : ValueOf](m: M, n: N) = new Canvas[M, N](
      r = alg.zero[M, N],
      g = alg.zero[M, N],
      b = alg.zero[M, N]
    )
  }

}
