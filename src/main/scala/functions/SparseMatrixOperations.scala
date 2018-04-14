package functions

import data._
import data.matrix.data.column.{Column, ColumnValue}
import data.matrix.data.{MatrixElement, row}
import data.matrix.data.row.{Row, RowValue}

import scala.annotation.tailrec

trait SparseMatrixOperations[T[_], F] {
  def ***(A: T[F], B: T[F]): T[F]

  def +++(A: T[F], B: T[F]): T[F]

  def ***(A: T[F], b: List[F]): T[F]
}

object SparseMatrixOperations {
  def normalizeToSparseMatrix(v: List[Double]): SparseMatrix[Double] = {
    val toRowValues = Row(0, v.zipWithIndex.map(e => RowValue(e._2, e._1)))
    SparseMatrix(List(toRowValues), VectorType)
  }

  case class RowIterator[F: Fractional](index: Int, values: Iterator[RowValue[F]])


  case class ConcurrentColumnIterator[F: Fractional](first: RowValueWithIndex2[F],
                                                     second: RowValueWithIndex2[F])

  type RowParser[F] = Iterator[RowIterator[F]]

  def sparseMatrixOperations: SparseMatrixOperations[SparseMatrix, Double] = new SparseMatrixOperations[SparseMatrix, Double] {
    override def ***(A: SparseMatrix[Double], B: SparseMatrix[Double]): SparseMatrix[Double] = {
      def multiplyColumnWithRow[F: Fractional](row: List[MatrixElement[F]], column: List[MatrixElement[F]]): F = {
        val frac = implicitly[Fractional[F]]

        def go(currRow: List[MatrixElement[F]], currColumn: List[MatrixElement[F]], result: F): F = {
          if (currRow.isEmpty || currColumn.isEmpty) result
          else {
            (currRow.head.index, currColumn.head.index) match {
              case (i, j) if i == j =>
                val updatedResult = frac.plus(result, frac.times(currRow.head.value, currColumn.head.value))
                go(currRow.tail, currColumn.tail, updatedResult)
              case (i, j) if i < j =>
                go(currRow.tail, currColumn, result)
              case (i, j) if i > j =>
                go(currRow, currColumn.tail, result)
            }
          }
        }
        /*val allElements = row ::: column
        val groupedByIndices = allElements.groupBy(_.index).values.toList

        groupedByIndices.foldRight(frac.zero) { (curr, acc) =>
          curr.length match {
            case 1 => acc
            case 2 => frac.plus(acc, frac.times(curr.head.value, curr(1).value))
            case _ => acc
          }
        }*/
        go(row, column, frac.zero)
      }

      def go(rows: List[Row[Double]],
             columns: List[Column[Double]]): List[Row[Double]] = {
        val newResult = rows.map { e =>
          val values = columns
            .map(c => RowValue[Double](c.index, multiplyColumnWithRow(e.values, c.values)))
            .filterNot(_.value == 0.0)

          Row(e.index, values)
        }
        newResult
      }

      SparseMatrix(go(A.rows, B.asColumns), MultiplicationResult)
    }

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
                  .groupBy(_.index)
                  .values
                  .toList

                val newElements = groupedElements.map(r => r.reduce {
                  (el1, el2) =>
                    RowValue(el1.index, implicitly[Fractional[F]].plus(el1.value, el2.value))
                })

                val newRow = row.Row(currHeadFirst.index, newElements.sortBy(_.index))
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

    override def ***(A: SparseMatrix[Double], b: List[Double]): SparseMatrix[Double] = {

      sparseMatrixOperations.***(A, normalizeToSparseMatrix(b))
    }
  }
}