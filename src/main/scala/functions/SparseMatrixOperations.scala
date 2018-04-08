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

      val firstMatrixIterator = A.rows.map(r => r.values.toIterator).toIterator
      val secondMatrixIterator = B.rows.map(r => r.values.toIterator).toIterator

      //the idea would be the following:
      /*
      have a function with the parameters:
      1. firstMatrixIterator
      2. secondMatrixIterator
      3. firstMatrixIterator head buffer
      4. secondMatrixIterator head buffer
      5. curr row
      6. curr column

      The idea would be the following:
       */
      SparseMatrix(List.empty)

    }
  }
}