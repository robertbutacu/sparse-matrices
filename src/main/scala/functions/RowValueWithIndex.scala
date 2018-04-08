package functions

import data.RowValue
import functions.SparseMatrixOperations.RowIterator

case class RowValueWithIndex[F: Fractional](index: Int, value: RowValue[F])

object RowValueWithIndex {
  def apply[F: Fractional](rowIterator: RowIterator[F]): RowValueWithIndex[F] =
    RowValueWithIndex(rowIterator.index, rowIterator.values.next)
}