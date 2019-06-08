package io.tuliplogic.raytracer.drawing

import io.tuliplogic.linalg.MinimalMatrix.Dim
import io.tuliplogic.raytracer.drawing.colors.RGB

/**
  *
  * ray-tracer - 2019-06-08
  * Created with ♥ in Amsterdam
  */
object canvas {

  import io.tuliplogic.linalg.MinimalMatrix._
  type Canvas[M <: Dim, N <: Dim] = Matrix[RGB, M, N]

//  trait ColorMatrixAlgebra extends Matrixas
//  object Canvas {
//    def create[M <: Dim, N <: Dim] =
//  }
}
