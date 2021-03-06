package data

import data.matrix.data.column.{Column, ColumnValue}
import data.matrix.data.row.{Row, RowValue, RowValueWithIndex}
import functions.matrix.operations.liniar.system.gauss.seidel.Precision

case class SparseMatrix[F: Fractional](rows: List[Row[F]],
                                       rowsType: RowsType = Simple,
                                       matrixType: MatrixType = SparseMatrixType) {
  require(MatrixType.isValid(this))

  def maxByColumn: Int = rows.maxBy(_.values.length).values.length

  val transpose: List[Column[F]] = {
    val allElements = for {
      row <- rows
      value <- row.values
    } yield RowValueWithIndex(row.index, value)

    val groupedByColumn = allElements.groupBy(_.value.index).values.toList

    val mappedToColumns = for {
      column <- groupedByColumn
      mappedToColumn = Column(column.head.value.index, column.map(c => ColumnValue(c.rowIndex, c.value.value)))
    } yield mappedToColumn

    mappedToColumns.sortBy(_.index)
  }
}

object SparseMatrix {
  def areEqual(A: SparseMatrix[Double], B: SparseMatrix[Double], precision: Precision): Boolean = {
    def areElementsTheSame(f: List[RowValue[Double]], s: List[RowValue[Double]]): Boolean = {
      def round(n: Double) = Math.round(n * 10 * precision.magnitude) / 10 * precision.magnitude

      def areApproximatelyEqual(x: Double, y: Double) =
        x == y

      f.zip(s).forall(p => p._1.index == p._2.index && areApproximatelyEqual(round(p._1.value), round(p._2.value)))
    }

    A.rows.zip(B.rows).forall(r => r._1.index == r._2.index && areElementsTheSame(r._1.values, r._2.values))
  }
}