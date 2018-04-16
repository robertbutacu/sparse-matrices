package data.matrix.data.row

import functions.SparseMatrixOperations.RowIterator

case class RowValueWithIndex[F: Fractional](rowIndex: Int, value: RowValue[F])

object RowValueWithIndex {
  def apply[F: Fractional](rowIterator: RowIterator[F]): RowValueWithIndex[F] =
    RowValueWithIndex(rowIterator.index, rowIterator.values.next)
}