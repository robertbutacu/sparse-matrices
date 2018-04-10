package functions

import data.matrix.data.row.RowValue
import functions.SparseMatrixOperations.RowIterator

case class RowValueWithIndex2[F: Fractional](index: Int, value: RowValue[F])

object RowValueWithIndex2 {
  def apply[F: Fractional](rowIterator: RowIterator[F]): RowValueWithIndex2[F] =
    RowValueWithIndex2(rowIterator.index, rowIterator.values.next)
}