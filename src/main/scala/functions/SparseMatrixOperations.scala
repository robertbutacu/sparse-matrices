package functions

import data.{AdditionResult, Row, RowValue, SparseMatrix}

import scala.annotation.tailrec

trait SparseMatrixOperations[T[_], F] {
  def ***(A: T[F], B: T[F]): T[F]

  def +++(A: T[F], B: T[F]): T[F]

  def ***(A: T[F], b: List[F]): T[F]
}

object SparseMatrixOperations {

  case class RowIterator[F: Fractional](index: Int, values: Iterator[RowValue[F]])


  case class ConcurrentColumnIterator[F: Fractional](first: RowValueWithIndex2[F],
                                                     second: RowValueWithIndex2[F])

  type RowParser[F] = Iterator[RowIterator[F]]

  def sparseMatrixOperations = new SparseMatrixOperations[SparseMatrix, Double] {
    override def ***(A: SparseMatrix[Double], B: SparseMatrix[Double]): SparseMatrix[Double] = ???

    override def +++(A: SparseMatrix[Double], B: SparseMatrix[Double]): SparseMatrix[Double] = {
      @tailrec
      def go[F: Fractional](firstMatrixRows: List[Row[F]],
                            secondMatrixRows: List[Row[F]],
                            result: List[Row[F]]): List[Row[F]] = {
        (firstMatrixRows.isEmpty, secondMatrixRows.isEmpty) match {
          case (true, true) => result
          case (false, true) => result ::: firstMatrixRows
          case (true, false) => result ::: secondMatrixRows
          case (false, false) =>
            val currHeadFirst = firstMatrixRows.head
            val currHeadSecond = secondMatrixRows.head

            (currHeadFirst.index, currHeadSecond.index) match {
              case (i, j) if i == j =>
                val groupedElements = (currHeadFirst.values ::: currHeadSecond.values)
                  .groupBy(_.columnIndex)
                  .values
                  .toList

                val newElements = groupedElements.map(r => r.reduce {
                  (el1, el2) =>
                    RowValue(el1.columnIndex, implicitly[Fractional[F]].plus(el1.value, el2.value))
                })

                val newRow = Row(currHeadFirst.index, newElements.sortBy(_.columnIndex))
                go(firstMatrixRows.tail, secondMatrixRows.tail, result :+ newRow)
              case (i, j) if i < j =>
                go(firstMatrixRows.tail, secondMatrixRows, result :+ firstMatrixRows.head)
              case (i, j) if i > j =>
                go(firstMatrixRows, secondMatrixRows.tail, result :+ secondMatrixRows.head)
            }
        }
      }

      SparseMatrix(go(A.rows, B.rows, List.empty), AdditionResult)
    }

    override def ***(A: SparseMatrix[Double], b: List[Double]): SparseMatrix[Double] = ???
  }
}