package functions

import data.SparseMatrix

trait SparseMatrixOperations {
  def ***[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
  def +++[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
}

object SparseMatrixOperations {
  def sparseMatrixOperations: SparseMatrixOperations = new SparseMatrixOperations {
    override def ***[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F] = ???

    override def +++[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F] = {
      val nrOfColumns = Math.max(A.maxByColumn, B.maxByColumn)
      val nrOfRows = Math.max(A.rows.length, B.rows.length)

      val firstMatrixIterator = A.rows.toIterator
      val secondMatrixIterator = B.rows.toIterator

      SparseMatrix(List.empty)

    }
  }
}