package functions

import data.{RowValue, SparseMatrix}

trait SparseMatrixOperations {
  def ***[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
  def +++[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
  def ***[F: Fractional](A: SparseMatrix[F], b: List[F]): SparseMatrix[F]
}

object SparseMatrixOperations {
  case class RowIterator[F: Fractional](index: Int, values: Iterator[RowValue[F]])
  case class ConcurrentRowIterator[F: Fractional](first: Iterator[RowValue[F]], second: Iterator[RowValue[F]])
  case class ConcurrentColumnIterator[F: Fractional](first: RowValue[F], second: RowValue[F])

  type RowParser[F: Fractional] = Iterator[RowIterator[F]]

  def sparseMatrixOperations: SparseMatrixOperations = new SparseMatrixOperations {
    override def ***[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F] = ???

    override def +++[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F] = {
      val nrOfColumns = Math.max(A.maxByColumn, B.maxByColumn)
      val nrOfRows = Math.max(A.rows.length, B.rows.length)


      def addMatrices(firstMatrix: RowParser[F], secondMatrix: RowParser[F], currentIterator: ConcurrentRowIterator[F],
                      concurrentColumnIterator: ConcurrentColumnIterator[F],
                      currRow: Int, currColumn: Int): SparseMatrix[F] = {
        if(currRow == nrOfRows && currColumn == nrOfColumns)
          SparseMatrix(List.empty)
        else {
          SparseMatrix(List.empty)
        }
      }

      val firstMatrixIterator = A.rows.map(r => RowIterator(r.index, r.values.toIterator)).toIterator
      val secondMatrixIterator = B.rows.map(r =>RowIterator(r.index, r.values.toIterator)).toIterator

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
      2. work with concurrentColumnIterator => compare that with currRow and currColumn
        => whenever the elements match, add them to the result, and pull a new value from the concurrentRowIterator
      3. now, when the column is done, next row => means replace concurrentRowIterator => FOR BOTH VALUES
      4. repeat until parsed everything, wrap the result into a matrix
       */
      SparseMatrix(List.empty)
    }

    override def ***[F: Fractional](A: SparseMatrix[F], b: List[F]): SparseMatrix[F] = ???
  }
}