package data.matrix.data.row

import functions.SparseMatrixOperations.RowIterator

case class RowValueWithIndex2[F: Fractional](rowIndex: Int, value: RowValue[F])

object RowValueWithIndex2 {
  def apply[F: Fractional](rowIterator: RowIterator[F]): RowValueWithIndex2[F] =
    RowValueWithIndex2(rowIterator.index, rowIterator.values.next)
}