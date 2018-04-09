package functions

import data.{RowValue, SparseMatrix}

trait SparseMatrixOperations[T[_], F] {
  def ***(A: T[F], B: T[F]): T[F]

  def +++(A: T[F], B: T[F]): T[F]

  def ***(A: T[F], b: List[F]): T[F]
}

object SparseMatrixOperations {

  case class RowIterator[F: Fractional](index: Int, values: Iterator[RowValue[F]])


  case class ConcurrentColumnIterator[F: Fractional](first: RowValueWithIndex[F],
                                                     second: RowValueWithIndex[F])

  type RowParser[F] = Iterator[RowIterator[F]]

  def sparseMatrixOperations = new SparseMatrixOperations[SparseMatrix, Double] {
    override def ***(A: SparseMatrix[Double], B: SparseMatrix[Double]): SparseMatrix[Double] = ???

    override def +++(A: SparseMatrix[Double], B: SparseMatrix[Double]): SparseMatrix[Double] = {
      val nrOfColumns = Math.max(A.maxByColumn, B.maxByColumn)
      val nrOfRows = Math.max(A.rows.length, B.rows.length)


      SparseMatrix(List.empty)
    }

    override def ***(A: SparseMatrix[Double], b: List[Double]): SparseMatrix[Double] = ???
  }
}