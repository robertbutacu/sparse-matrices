package functions

import data.{Row, RowValue, SparseMatrix}

trait SparseMatrixOperations {
  def ***[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
  def +++[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
}

object SparseMatrixOperations {
  case class IteratorRow[F: Fractional](index: Int, values: Iterator[RowValue[F]])
  case class ConcurrentIterator[F: Fractional](first: Iterator[RowValue[F]], second: Iterator[RowValue[F]])

  type RowParser[F: Fractional] = Iterator[IteratorRow[F]]

  def sparseMatrixOperations: SparseMatrixOperations = new SparseMatrixOperations {
    override def ***[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F] = ???

    override def +++[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F] = {
      def addMatrices(firstMatrix: RowParser[F], secondMatrix: RowParser[F], currentIterator: ConcurrentIterator[F],
                      currRow: Int, currColumn: Int): SparseMatrix[F] = {
        SparseMatrix(List.empty)
      }


      val nrOfColumns = Math.max(A.maxByColumn, B.maxByColumn)
      val nrOfRows = Math.max(A.rows.length, B.rows.length)

      val firstMatrixIterator = A.rows.map(r => IteratorRow(r.index, r.values.toIterator)).toIterator
      val secondMatrixIterator = B.rows.map(r =>IteratorRow(r.index, r.values.toIterator)).toIterator

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
      1. pull a new row when the curr column has run out of elements

       */
      SparseMatrix(List.empty)

    }
  }
}