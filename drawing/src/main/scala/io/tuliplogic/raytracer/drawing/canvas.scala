package io.tuliplogic.raytracer.drawing

import io.tuliplogic.linalg.{Dim, MatrixBox}
import singleton.ops.{<, Require}


class Canvas[
  T,
  Box[_, _ <: Dim, _ <: Dim],
  M <: Dim : ValueOf,
  N <: Dim : ValueOf
](
  val grid: Box[T, M, N]
)

object Canvas {
  implicit def canvasBox[
    T,
    Matrix[_, _ <: Dim, _ <: Dim],
    M <: Dim : ValueOf,
    N <: Dim : ValueOf
  ](implicit box: MatrixBox[Matrix, T]):
  MatrixBox[λ[(TT, `MM <: Dim`, `NN <: Dim`) => Canvas[TT, Matrix, MM, NN]], (T, T, T)] =
    new MatrixBox[λ[(TT, `MM <: Dim`, `NN <: Dim`) => Canvas[TT, Matrix, MM, NN]], (T, T, T)] {

      override def col[M <: Dim : ValueOf](ts: Seq[(T, T, T)]): ColVector[M] = new Canvas[(T, T, T), Matrix, ]()

      override def row[N <: Dim : ValueOf](ts: Seq[(T, T, T)]): RowVector[N] = ???

      /**
        * Create a matrix populating it with the elements from the provided sequence, row by row.
        * If {{ts.size > M * N}} the elements in excess are discarded
        * If {{ts.size < M * N}} the missing elements are filled with zero
        */
      override def create[M <: Dim : ValueOf, N <: Dim : ValueOf](ts: (T, T, T)*): Canvas[(T, T, T), Matrix, M, N] = ???

      override def fill[M <: Dim : ValueOf, N <: Dim : ValueOf](t: (T, T, T)): Canvas[(T, T, T), Matrix, M, N] = ???

      override def getRow[M <: Dim : ValueOf, N <: Dim : ValueOf, Row <: Dim : ValueOf](m1: Canvas[(T, T, T), Matrix, M, N])(m: Row)(implicit lteq: Require[Row < M]): RowVector[N] = ???

      override def getCol[M <: Dim : ValueOf, N <: Dim : ValueOf, Col <: Dim : ValueOf](m1: Canvas[(T, T, T), Matrix, M, N])(n: Col)(implicit lteq: Require[Col < N]): ColVector[M] = ???

      override def transpose[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Canvas[(T, T, T), Matrix, M, N]): Canvas[(T, T, T), Matrix, N, M] = ???

      override def value(m: Canvas[(T, T, T), Matrix, 1, 1]): (T, T, T) = ???

      override def values[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Canvas[(T, T, T), Matrix, M, N]): List[(T, T, T)] = ???
    }

}