package functions

import functions.SparseMatrixOperations.RowIterator

case class ConcurrentRowIterator[F: Fractional](first: Iterator[RowValueWithIndex[F]],
                                                second: Iterator[RowValueWithIndex[F]])

object ConcurrentRowIterator {
  def apply[F: Fractional](first: RowIterator[F], second: RowIterator[F]): ConcurrentRowIterator[F] = {
    ConcurrentRowIterator(first.values.map(r => RowValueWithIndex(first.index, r)),
      second.values.map(r => RowValueWithIndex(second.index, r)))
  }
}
