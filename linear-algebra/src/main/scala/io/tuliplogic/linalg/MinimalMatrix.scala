package io.tuliplogic.linalg

import cats.kernel.Eq
import singleton.ops._

trait MinimalMatrix {

  type Dim = XInt

  /**
    * A matrix with M rows and N cols
    * No assumption is made on the T type, although for most of matrix operations it will be required to be a Field
    */
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
    def create[M <: Dim : ValueOf, N <: Dim : ValueOf](ts: T*): Matrix[T, M, N]
    def createRow[N <: Dim : ValueOf](ts: T*): Matrix[T, 1, N] = create[1, N](ts: _*)
    def createCol[M <: Dim : ValueOf](ts: T*): Matrix[T, M, 1] = create[M, 1](ts: _*)

    def plus[M <: Dim : ValueOf, N <: Dim : ValueOf](m1: Matrix[T, M, N], m2: Matrix[T, M, N]): Matrix[T, M, N]

    def negate[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Matrix[T, M, N]): Matrix[T, M, N]

    def scale[M <: Dim : ValueOf, N <: Dim : ValueOf](α: T)(m: Matrix[T, M, N]): Matrix[T, M, N]

    def times[M <: Dim : ValueOf, N <: Dim : ValueOf, P <: Dim : ValueOf]
      (m1: Matrix[T, M, N], m2: Matrix[T, N, P]): Matrix[T, M, P]

    def getRow[M <: Dim : ValueOf, N <: Dim : ValueOf, Row <: Dim : ValueOf](m1: Matrix[T, M, N])(m: Row)
      (implicit lteq: Require[Row < M]): RowVector[N]

    def getCol[M <: Dim : ValueOf, N <: Dim : ValueOf, Col <: Dim : ValueOf](m1: Matrix[T, M, N])(n: Col)
      (implicit lteq: Require[Col < N]): ColVector[M]

    def transpose[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Matrix[T, M, N]): Matrix[T, N, M]

    def value(m: Matrix[T, 1, 1]): T

    def values[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Matrix[T, M, N]): List[T]

    def get[M <: Dim : ValueOf, N <: Dim : ValueOf, I <: Dim : ValueOf, J <: Dim : ValueOf](m: Matrix[T, M, N])(i: I, j: J)
      (implicit lteqRow: Require[I < M], lteqCol: Require[J < N]): T = {
      val iThRow: RowVector[N] = getRow(m)(i)
      val jThCol: ColVector[1] = getCol[1, N, J](iThRow)(j)
      value(jThCol)
    }

    //vector operations
    def dot[M <: Dim : ValueOf](v: ColVector[M], w: ColVector[M]): T = value(times(transpose(v), w))

    def cross(v: ColVector[3], w: ColVector[3]): ColVector[3]

    def norm[M <: Dim : ValueOf](v: ColVector[M]): T
    def normalize[M <: Dim : ValueOf]: T
  }

  object syntax {
    import cats.implicits._

    implicit def matrixEq[T: Eq, M <: Dim : ValueOf, N <: Dim : ValueOf](implicit alg: MatrixAlgebra[T]): Eq[Matrix[T, M, N]] =
      new Eq[Matrix[T, M, N]] {
        override def eqv(x: Matrix[T, M, N], y: Matrix[T, M, N]): Boolean =
          alg.values(x) === alg.values(y)
      }

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
        alg.getRow(m)(row)
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




