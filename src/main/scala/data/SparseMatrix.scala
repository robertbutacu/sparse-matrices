package data

import data.matrix.data.column.{Column, ColumnValue}
import data.matrix.data.row.{Row, RowValue, RowValueWithIndex}

case class SparseMatrix[F: Fractional](rows: List[Row[F]], matrixType: MatrixType = Simple) {
  require(rows.forall(r => r.values.length <= MatrixType.maximumLength(matrixType)))

  def maxByColumn: Int = rows.maxBy(_.values.length).values.length

  val asColumns: List[Column[F]] = {
    val allElements = for {
      row <- rows
      value <- row.values
    } yield RowValueWithIndex(value.value, row.index, value.index)

    val sorted = allElements.sortBy(_.columnIndex)

    val groupedByColumn = sorted.groupBy(_.columnIndex).values.toList

    val mappedToColumns = for {
      column <- groupedByColumn
      mappedToColumn = Column(column.head.columnIndex, column.map(c => ColumnValue(c.rowIndex, c.value)))
    } yield mappedToColumn

    mappedToColumns.sortBy(_.index)
  }
}

object SparseMatrix {
  def equals(A: SparseMatrix[Double], B: SparseMatrix[Double]): Boolean = {
    def areElementsTheSame(f: List[RowValue[Double]], s: List[RowValue[Double]]): Boolean = {
      def round(n: Double) = Math.round(n * 100) / 100
      f.zip(s).forall(p => p._1.index == p._2.index && round(p._1.value) == round(p._2.value))
    }
    A.rows.zip(B.rows).forall(r => r._1.index == r._2.index && areElementsTheSame(r._1.values, r._2.values))
  }
}