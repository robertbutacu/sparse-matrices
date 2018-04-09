package functions

import functions.SparseMatrixOperations.RowIterator

case class ConcurrentRowIterator[F: Fractional](first: Iterator[RowValueWithIndex2[F]],
                                                second: Iterator[RowValueWithIndex2[F]])

object ConcurrentRowIterator {
  def apply[F: Fractional](first: RowIterator[F], second: RowIterator[F]): ConcurrentRowIterator[F] = {
    ConcurrentRowIterator(first.values.map(r => RowValueWithIndex2(first.index, r)),
      second.values.map(r => RowValueWithIndex2(second.index, r)))
  }
}
