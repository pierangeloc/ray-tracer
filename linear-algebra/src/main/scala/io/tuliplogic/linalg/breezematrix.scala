package io.tuliplogic.linalg

import breeze.linalg.DenseMatrix
import breeze.linalg.operators.{OpAdd, OpMulMatrix, OpMulScalar}
import breeze.math.Field
import breeze.storage.Zero
import singleton.ops._

import scala.reflect.ClassTag

object breezematrix extends MinimalMatrix {

  class BreezeMatrix[T, M <: Dim : ValueOf, N <: Dim : ValueOf](private[linalg] val backing: DenseMatrix[T]) {
    override def toString() = backing.toString()
  }

  override type Matrix[T, M <: Dim, N <: Dim] =
    BreezeMatrix[T, M, N]

  def matrixAlgebra[T: Zero : ClassTag](
    implicit opAdd: OpAdd.Impl2[DenseMatrix[T], DenseMatrix[T],DenseMatrix[T]],
    opMulScalar: OpMulScalar.Impl2[DenseMatrix[T], T, DenseMatrix[T]],
    opMul: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
    fe: Field[T]
  ): breezematrix.MatrixAlgebra[T] = new MatrixAlgebra[T] {

    override def zero[M <: Dim : ValueOf, N <: Dim : ValueOf]: Matrix[T, M, N] =
      new BreezeMatrix[T, M, N](DenseMatrix.zeros[T](valueOf[M], valueOf[N]))

    override def ones[M <: Dim : ValueOf, N <: Dim : ValueOf]: Matrix[T, M, N] =
      new BreezeMatrix[T, M, N](DenseMatrix.ones[T](valueOf[M], valueOf[N]))

    override def col[M <: Dim : ValueOf](ts: Seq[T]): ColVector[M] =
      new BreezeMatrix[T, M, 1](DenseMatrix.create(valueOf[M], 1, ts.toArray))

    override def row[N <: Dim : ValueOf](ts: Seq[T]): RowVector[N] =
      new BreezeMatrix[T, 1, N](DenseMatrix.create(1, valueOf[N], ts.toArray))

    /**
      * Create a matrix populating it with the elements from the provided sequence, row by row.
      * If {{ts.size > M * N}} the elements in excess are discarded
      * If {{ts.size < M * N}} the missing elements are filled with zero
      */
    override def create[M <: Dim : ValueOf, N <: Dim : ValueOf](ts: T*): Matrix[T, M, N] =
      new BreezeMatrix[T, M, N](DenseMatrix.create(valueOf[M], valueOf[N], ts.toArray))

    override def plus[M <: Dim : ValueOf, N <: Dim : ValueOf](m1: Matrix[T, M, N], m2: Matrix[T, M, N]): Matrix[T, M, N] =
      new BreezeMatrix[T, M, N](m1.backing + m2.backing)

    override def negate[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Matrix[T, M, N]): Matrix[T, M, N] =
      new BreezeMatrix[T, M, N](-m.backing)

    override def scale[M <: Dim : ValueOf, N <: Dim : ValueOf](α: T)(m: Matrix[T, M, N]): Matrix[T, M, N] =
      new BreezeMatrix[T, M, N](m.backing *:* α)

    override def times[M <: Dim : ValueOf, N <: Dim : ValueOf, P <: Dim : ValueOf](m1: Matrix[T, M, N], m2: Matrix[T, N, P]): Matrix[T, M, P] =
      new BreezeMatrix[T, M, P](m1.backing * m2.backing)

    override def row[M <: Dim : ValueOf, N <: Dim : ValueOf, Row <: Dim : ValueOf](m1: Matrix[T, M, N])(m: Row)
      (implicit lteq: Require[Row < M]): RowVector[N] =
      new BreezeMatrix[T, 1, N](m1.backing(m, 0 until valueOf[N]).t.toDenseMatrix)

    override def col[M <: Dim : ValueOf, N <: Dim : ValueOf, Col <: Dim : ValueOf](m1: Matrix[T, M, N])(n: Col)
      (implicit lteq: Require[Col < N]): ColVector[M] =
      new BreezeMatrix[T, M, 1](m1.backing(0 until valueOf[M], n).toDenseMatrix)

    override def value(m: Matrix[T, 1, 1]): T = m.backing.valueAt(0)
  }

  val doubleMatrixAlgebra: breezematrix.MatrixAlgebra[Double] = matrixAlgebra[Double]
  val bigDecimalAlgebra: breezematrix.MatrixAlgebra[BigDecimal] = matrixAlgebra[BigDecimal]

}




