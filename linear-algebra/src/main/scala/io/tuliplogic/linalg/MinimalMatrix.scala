package io.tuliplogic.linalg

import breeze.math.Field
import singleton.ops._


trait MinimalMatrix {

  type Dim = XInt

  type Matrix[T, M <: Dim, N <: Dim]

  trait MatrixAlgebra[T] {

    type RowVector[N <: Dim] = Matrix[T, 1, N]
    type ColVector[M <: Dim] = Matrix[T, M, 1]

    def zero[M <: Dim : ValueOf, N <: Dim : ValueOf]: Matrix[T, M, N]

    def ones[M <: Dim : ValueOf, N <: Dim : ValueOf]: Matrix[T, M, N]

    def col[M <: Dim : ValueOf](ts: Seq[T]): ColVector[M]
    def row[N <: Dim : ValueOf](ts: Seq[T]): RowVector[N]

    /**
      * Create a matrix populating it with the elements from the provided sequence, row by row.
      * If {{ts.size > M * N}} the elements in excess are discarded
      * If {{ts.size < M * N}} the missing elements are filled with zero
      */
    def create[M <: Dim : ValueOf, N <: Dim : ValueOf](ts: Seq[T]): Matrix[T, M, N]

    def plus[M <: Dim : ValueOf, N <: Dim : ValueOf](m1: Matrix[T, M, N], m2: Matrix[T, M, N]): Matrix[T, M, N]

    def negate[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Matrix[T, M, N]): Matrix[T, M, N]

    def scale[M <: Dim : ValueOf, N <: Dim : ValueOf](α: T)(m: Matrix[T, M, N]): Matrix[T, M, N]

    def times[M <: Dim : ValueOf, N <: Dim : ValueOf, P <: Dim : ValueOf]
      (m1: Matrix[T, M, N], m2: Matrix[T, N, P]): Matrix[T, M, P]

    def row[M <: Dim : ValueOf, N <: Dim : ValueOf, Row <: Dim : ValueOf](m1: Matrix[T, M, N])(m: Row)
      (implicit lteq: Require[Row < M]): RowVector[N]

    def col[M <: Dim : ValueOf, N <: Dim : ValueOf, Col <: Dim : ValueOf](m1: Matrix[T, M, N])(n: Col)
      (implicit lteq: Require[Col < N]): ColVector[M]

    def value(m: Matrix[T, 1, 1]): T

  }

  object syntax {

    implicit class RichMatrix[T, M <: Dim : ValueOf, N <: Dim : ValueOf](m: Matrix[T, M, N])
      (implicit val alg: MatrixAlgebra[T]) {
      def *[P <: Dim : ValueOf](other: Matrix[T, N, P]): Matrix[T, M, P] =
        alg.times(m, other)

      def +(other: Matrix[T, M, N]): Matrix[T, M, N] =
        alg.plus(m, other)

      def unary_-(): Matrix[T, M, N] =
        alg.negate(m)

      def *(α: T): Matrix[T, M, N] =
        alg.scale(α)(m)

      def row[Row <: Dim : ValueOf](row: Row)(implicit lteq: Require[Row < M]): alg.RowVector[N] =
        alg.row(m)(row)
    }

    implicit class RichSingletonMatrix[T](m: Matrix[T, 1, 1])
      (implicit alg: MatrixAlgebra[T]) {
      def value: T = alg.value(m)
    }

    implicit class RichScalar[T, M <: Dim : ValueOf, N <: Dim : ValueOf](α: T)
      (implicit alg: MatrixAlgebra[T]) {
      def *(m: Matrix[T, M, N]): Matrix[T, M, N] =
        alg.scale(α)(m)
    }

  }

}




