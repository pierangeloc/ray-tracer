package io.tuliplogic.linalg

import breeze.linalg.DenseMatrix
import breeze.linalg.operators.{OpAdd, OpMulMatrix, OpMulScalar}
import breeze.linalg._
import breeze.linalg.norm._
import breeze.math.Field
import breeze.storage.Zero
import singleton.ops._

import scala.reflect.ClassTag

object breezematrix {
  class Mat[T, M <: Dim : ValueOf, N <: Dim : ValueOf](private[linalg] val backing: DenseMatrix[T]) {
    override def toString() = backing.toString()
  }

  def matrixAlgebra[T: Zero : ClassTag](
    implicit opAdd: OpAdd.Impl2[DenseMatrix[T], DenseMatrix[T],DenseMatrix[T]],
    opMulScalar: OpMulScalar.Impl2[DenseMatrix[T], T, DenseMatrix[T]],
    opMulScalarDouble: OpMulScalar.Impl2[DenseMatrix[T], Double, DenseMatrix[T]],
    opMul: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
    fe: Field[T]
  ): MatrixAlgebra[Mat, T] = new MatrixAlgebra[Mat, T] {

    /**
      * Create a matrix populating it with the elements from the provided sequence, row by row.
      * If {{ts.size > M * N}} the elements in excess are discarded
      * If {{ts.size < M * N}} the missing elements are filled with zero
      */
    override def create[M <: Dim : ValueOf, N <: Dim : ValueOf](ts: T*): Mat[T, M, N] =
      new Mat[T, M, N](DenseMatrix.create(valueOf[M], valueOf[N], ts.toArray, 0, valueOf[N], true))

    def fill[M <: Dim : ValueOf, N <: Dim : ValueOf](t: T): Mat[T, M, N] =
      new Mat[T, M, N](DenseMatrix.fill(valueOf[M], valueOf[N])(t))

    override def getRow[M <: Dim : ValueOf, N <: Dim : ValueOf, Row <: Dim : ValueOf](m1: Mat[T, M, N])(m: Row)
      (implicit lteq: Require[Row < M]): RowVector[N] =
      new Mat[T, 1, N](m1.backing(m, 0 until valueOf[N]).t.toDenseMatrix)

    override def getCol[M <: Dim : ValueOf, N <: Dim : ValueOf, Col <: Dim : ValueOf](m1: Mat[T, M, N])(n: Col)
      (implicit lteq: Require[Col < N]): ColVector[M] =
      new Mat[T, M, 1](m1.backing(0 until valueOf[M], n).toDenseMatrix)

    override def transpose[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Mat[T, M, N]): Mat[T, N, M] =
      new Mat[T, N, M](m.backing.t)

    override def value(m: Mat[T, 1, 1]): T = m.backing.valueAt(0)

    override def values[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Mat[T, M, N]): List[T] = m.backing.valuesIterator.toList
    override def update[M <: Dim : ValueOf, N <: Dim : ValueOf, I <: Dim : ValueOf, J <: Dim : ValueOf]
      (m: Matrix[T, M, N], i: I, j: J, v: T)
      (implicit lteqRow: Require[I < M], lteqCol: Require[J < N]): Mat[T, M, N] = {
      val copy = m.backing.copy
      copy.update(i, j, v)
      new Mat[T, M, N](copy)
    }



    override def zero[M <: Dim : ValueOf, N <: Dim : ValueOf]: Mat[T, M, N] =
    new Mat[T, M, N](DenseMatrix.zeros[T](valueOf[M], valueOf[N]))

    def identity[M <: Dim : ValueOf]: Mat[T, M, M] =
    new Mat[T, M, M](DenseMatrix.eye(valueOf[M]))

    override def ones[M <: Dim : ValueOf, N <: Dim : ValueOf]: Mat[T, M, N] =
    new Mat[T, M, N](DenseMatrix.ones[T](valueOf[M], valueOf[N]))

    override def col[M <: Dim : ValueOf](ts: Seq[T]): ColVector[M] =
    new Mat[T, M, 1](DenseMatrix.create(valueOf[M], 1, ts.toArray))

    override def row[N <: Dim : ValueOf](ts: Seq[T]): RowVector[N] =
    new Mat[T, 1, N](DenseMatrix.create(1, valueOf[N], ts.toArray))


    override def plus[M <: Dim : ValueOf, N <: Dim : ValueOf](m1: Mat[T, M, N], m2: Mat[T, M, N]): Mat[T, M, N] =
      new Mat[T, M, N](m1.backing + m2.backing)

    override def negate[M <: Dim : ValueOf, N <: Dim : ValueOf](m: Mat[T, M, N]): Mat[T, M, N] =
      new Mat[T, M, N](-m.backing)

    override def timesScalar[M <: Dim : ValueOf, N <: Dim : ValueOf](α: T)(m: Mat[T, M, N]): Mat[T, M, N] =
      new Mat[T, M, N](m.backing *:* α)

    override def scale[M <: Dim : ValueOf](α: Double)(v: ColVector[M]): ColVector[M] = new Mat[T, M, 1](v.backing *:* α)

    override def times[M <: Dim : ValueOf, N <: Dim : ValueOf, P <: Dim : ValueOf](m1: Mat[T, M, N], m2: Mat[T, N, P]): Mat[T, M, P] =
      new Mat[T, M, P](m1.backing * m2.backing)


    //(column) vector operations
    override def cross(v: ColVector[3], w: ColVector[3]): ColVector[3] = {
      import breeze.linalg._

      val List(v_x, v_y, v_z) = values(v)
      val List(w_x, w_y, w_z) = values(w)
      create[3, 1](
        v_y * w_z - v_z * w_y,
        v_z * w_x - v_x * w_z,
        v_x * w_y - v_y * w_x
      )
    }

    override def norm[M <: Dim : ValueOf](v: ColVector[M]): Double = breeze.linalg.norm(v.backing.toDenseVector)
  }

  implicit val doubleMatrixAlgebra: MatrixAlgebra[Mat, Double] = matrixAlgebra[Double]
//  implicit val bigDecimalAlgebra: breezematrix.MatrixAlgebra[BigDecimal] = matrixAlgebra[BigDecimal]

}




